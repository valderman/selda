{-# LANGUAGE OverloadedStrings, RecordWildCards, GADTs, CPP #-}
-- | PostgreSQL backend for Selda.
module Database.Selda.PostgreSQL
  ( PGConnectInfo (..)
  , withPostgreSQL, on, auth
  , pgBackend
  , pgConnString
  ) where
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Database.Selda.Backend
import Control.Monad.Catch

#ifndef __HASTE__
import Database.Selda.PostgreSQL.Encoding
import Database.PostgreSQL.LibPQ hiding (user, pass, db, host)
import qualified Data.ByteString.Char8 as BS
#endif

-- | PostgreSQL connection information.
data PGConnectInfo = PGConnectInfo
  { -- | Host to connect to.
    pgHost     :: T.Text
    -- | Port to connect to.
  , pgPort     :: Int
    -- | Name of database to use.
  , pgDatabase :: T.Text
    -- | Username for authentication, if necessary.
  , pgUsername :: Maybe T.Text
    -- | Password for authentication, if necessary.
  , pgPassword :: Maybe T.Text
  }

-- | Connect to the given database on the given host, on the default PostgreSQL
--   port (5432):
--
-- > withPostgreSQL ("my_db" `on` "example.com") $ do
-- >   ...
on :: T.Text -> T.Text -> PGConnectInfo
on db host = PGConnectInfo
  { pgHost = host
  , pgPort = 5432
  , pgDatabase = db
  , pgUsername = Nothing
  , pgPassword = Nothing
  }
infixl 7 `on`

-- | Add the given username and password to the given connection information:
--
-- > withPostgreSQL ("my_db" `on` "example.com" `auth` ("user", "pass")) $ do
-- >   ...
--
--   For more precise control over the connection options, you should modify
--   the 'PGConnectInfo' directly.
auth :: PGConnectInfo -> (T.Text, T.Text) -> PGConnectInfo
auth ci (user, pass) = ci
  { pgUsername = Just user
  , pgPassword = Just pass
  }
infixl 4 `auth`

-- | Perform the given computation over a PostgreSQL database.
--   The database connection is guaranteed to be closed when the computation
--   terminates.
withPostgreSQL :: (MonadIO m, MonadThrow m, MonadMask m)
               => PGConnectInfo -> SeldaT m a -> m a
#ifdef __HASTE__
withPostgreSQL _ _ = return $ error "withPostgreSQL called in JS context"
#else
withPostgreSQL ci m = do
  conn <- liftIO $ connectdb connstr
  st <- liftIO $ status conn
  case st of
    ConnectionOk -> do
      let backend = pgBackend (decodeUtf8 connstr) conn
      liftIO $ runStmt backend "SET client_min_messages TO WARNING;" []
      runSeldaT m backend `finally` liftIO (finish conn)
    nope -> do
      connFailed nope
  where
    connstr = pgConnString ci
    connFailed f = throwM $ DbError $ unwords
      [ "unable to connect to postgres server: " ++ show f
      ]

-- | Create a `SeldaBackend` for PostgreSQL `Connection`
pgBackend :: T.Text       -- ^ Unique database identifier. Preferably the
                          --   connection string used to open the connection.
          -> Connection   -- ^ PostgreSQL connection object.
          -> SeldaBackend
pgBackend ident c = SeldaBackend
  { runStmt        = \q ps -> right <$> pgQueryRunner c False q ps
  , runStmtWithPK  = \q ps -> left <$> pgQueryRunner c True q ps
  , ppConfig       = defPPConfig
    { ppType = pgColType defPPConfig
    , ppAutoIncInsert = "DEFAULT"
    , ppColAttrs = ppColAttrs defPPConfig . filter (/= AutoIncrement)
    }
  , dbIdentifier   = ident
  }
  where
    left (Left x) = x
    left _        = error "impossible"
    right (Right x) = x
    right _         = error "impossible"

-- | Convert `PGConnectInfo` into `ByteString`
pgConnString :: PGConnectInfo -> BS.ByteString
pgConnString PGConnectInfo{..} = mconcat
  [ "host=", encodeUtf8 pgHost, " "
  , "port=", BS.pack (show pgPort), " "
  , "dbname=", encodeUtf8 pgDatabase, " "
  , case pgUsername of
      Just user -> "user=" <> encodeUtf8 user <> " "
      _         -> ""
  , case pgPassword of
      Just pass -> "password=" <> encodeUtf8 pass <> " "
      _         -> ""
  , "connect_timeout=10", " "
  , "client_encoding=UTF8"
  ]

pgQueryRunner :: Connection -> Bool -> T.Text -> [Param] -> IO (Either Int (Int, [[SqlValue]]))
pgQueryRunner c return_lastid q ps = do
    mres <- execParams c (encodeUtf8 q') [fromSqlValue p | Param p <- ps] Text
    case mres of
      Just res -> do
        st <- resultStatus res
        case st of
          BadResponse       -> throwM $ SqlError "bad response"
          FatalError        -> throwM $ SqlError errmsg
          NonfatalError     -> throwM $ SqlError errmsg
          _ | return_lastid -> Left <$> getLastId res
            | otherwise     -> Right <$> getRows res
      Nothing           -> throwM $ DbError "unable to submit query to server"
  where
    errmsg = "error executing query `" ++ T.unpack q' ++ "'"
    q' | return_lastid = q <> " RETURNING LASTVAL();"
       | otherwise     = q

    getLastId res = (readInt . maybe "0" id) <$> getvalue res 0 0

    getRows res = do
      rows <- ntuples res
      cols <- nfields res
      types <- mapM (ftype res) [0..cols-1]
      affected <- cmdTuples res
      result <- mapM (getRow res types cols) [0..rows-1]
      pure $ case affected of
        Just "" -> (0, result)
        Just s  -> (readInt s, result)
        _       -> (0, result)

    getRow res types cols row = do
      sequence $ zipWith (getCol res row) [0..cols-1] types

    getCol res row col t = do
      mval <- getvalue res row col
      case mval of
        Just val -> pure $ toSqlValue t val
        _        -> pure SqlNull

-- | Custom column types for postgres.
pgColType :: PPConfig -> SqlTypeRep -> T.Text
pgColType _ TRowID    = "BIGSERIAL"
pgColType _ TInt      = "INT8"
pgColType _ TFloat    = "FLOAT8"
pgColType _ TDateTime = "TIMESTAMP"
pgColType _ TBlob     = "BYTEA"
pgColType cfg t       = ppType cfg t
#endif
