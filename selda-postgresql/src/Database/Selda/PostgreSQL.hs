{-# LANGUAGE OverloadedStrings, RecordWildCards, GADTs, CPP #-}
-- | PostgreSQL backend for Selda.
module Database.Selda.PostgreSQL
  ( PGConnectInfo (..)
  , withPostgreSQL, on, auth
  , pgOpen, pgOpen', seldaClose
  , pgConnString
  ) where
import qualified Data.ByteString.Char8 as BS
import Data.Dynamic
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Database.Selda.Backend
import Control.Monad.Catch
import Control.Monad.IO.Class

#ifndef __HASTE__
import Database.Selda.PostgreSQL.Encoding
import Database.PostgreSQL.LibPQ hiding (user, pass, db, host)
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

-- | Convert `PGConnectInfo` into `ByteString`
pgConnString :: PGConnectInfo -> BS.ByteString
#ifdef __HASTE__
pgConnString PGConnectInfo{..} = error "pgConnString called in JS context"
#else
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
#endif

-- | Perform the given computation over a PostgreSQL database.
--   The database connection is guaranteed to be closed when the computation
--   terminates.
withPostgreSQL :: (MonadIO m, MonadThrow m, MonadMask m)
               => PGConnectInfo -> SeldaT m a -> m a
#ifdef __HASTE__
withPostgreSQL _ _ = return $ error "withPostgreSQL called in JS context"
#else
withPostgreSQL ci m = bracket (pgOpen ci) seldaClose (runSeldaT m)
#endif

-- | Open a new PostgreSQL connection. The connection will persist across
--   calls to 'runSeldaT', and must be explicitly closed using 'seldaClose'
--   when no longer needed.
pgOpen :: (MonadIO m, MonadMask m) => PGConnectInfo -> m SeldaConnection
#ifdef __HASTE__
pgOpen _ = return $ error "pgOpen called in JS context"
#else
pgOpen ci = pgOpen' $ pgConnString ci

pgOpen' :: (MonadIO m, MonadMask m) => BS.ByteString -> m SeldaConnection
pgOpen' connStr =
  bracketOnError (liftIO $ connectdb connStr) (liftIO . finish) $ \conn -> do
    st <- liftIO $ status conn
    case st of
      ConnectionOk -> do
        let backend = pgBackend conn
        liftIO $ runStmt backend "SET client_min_messages TO WARNING;" []
        newConnection backend (decodeUtf8 connStr)
      nope -> do
        connFailed nope
    where
      connFailed f = throwM $ DbError $ unwords
        [ "unable to connect to postgres server: " ++ show f
        ]

-- | Create a `SeldaBackend` for PostgreSQL `Connection`
pgBackend :: Connection   -- ^ PostgreSQL connection object.
          -> SeldaBackend
pgBackend c = SeldaBackend
  { runStmt         = \q ps -> right <$> pgQueryRunner c False q ps
  , runStmtWithPK   = \q ps -> left <$> pgQueryRunner c True q ps
  , prepareStmt     = pgPrepare c
  , runPrepared     = pgRun c
  , backendId       = PostgreSQL
  , ppConfig        = defPPConfig
    { ppType = pgColType defPPConfig
    , ppAutoIncInsert = "DEFAULT"
    , ppColAttrs = ppColAttrs defPPConfig . filter (/= AutoIncrement)
    }
  , closeConnection = \_ -> finish c
  }
  where
    left (Left x) = x
    left _        = error "impossible"
    right (Right x) = x
    right _         = error "impossible"

pgQueryRunner :: Connection -> Bool -> T.Text -> [Param] -> IO (Either Int (Int, [[SqlValue]]))
pgQueryRunner c return_lastid q ps = do
    mres <- execParams c (encodeUtf8 q') [fromSqlValue p | Param p <- ps] Text
    unlessError c errmsg mres $ \res -> do
      if return_lastid
        then Left <$> getLastId res
        else Right <$> getRows res
  where
    errmsg = "error executing query `" ++ T.unpack q' ++ "'"
    q' | return_lastid = q <> " RETURNING LASTVAL();"
       | otherwise     = q

    getLastId res = (readInt . maybe "0" id) <$> getvalue res 0 0

pgRun :: Connection -> Dynamic -> [Param] -> IO (Int, [[SqlValue]])
pgRun c hdl ps = do
    let Just sid = fromDynamic hdl :: Maybe StmtID
    mres <- execPrepared c (BS.pack $ show sid) (map mkParam ps) Text
    unlessError c errmsg mres $ getRows
  where
    errmsg = "error executing prepared statement"
    mkParam (Param p) = case fromSqlValue p of
      Just (_, val, fmt) -> Just (val, fmt)
      Nothing            -> Nothing

-- | Get all rows from a result.
getRows :: Result -> IO (Int, [[SqlValue]])
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

-- | Get all columns for the given row.
getRow :: Result -> [Oid] -> Column -> Row -> IO [SqlValue]
getRow res types cols row = do
  sequence $ zipWith (getCol res row) [0..cols-1] types

-- | Get the given column.
getCol :: Result -> Row -> Column -> Oid -> IO SqlValue
getCol res row col t = do
  mval <- getvalue res row col
  case mval of
    Just val -> pure $ toSqlValue t val
    _        -> pure SqlNull

pgPrepare :: Connection -> StmtID -> [SqlTypeRep] -> T.Text -> IO Dynamic
pgPrepare c sid types q = do
    mres <- prepare c (BS.pack $ show sid) (encodeUtf8 q) (Just types')
    unlessError c errmsg mres $ \_ -> return (toDyn sid)
  where
    types' = map fromSqlType types
    errmsg = "error preparing query `" ++ T.unpack q ++ "'"

-- | Perform the given computation unless an error occurred previously.
unlessError :: Connection -> String -> Maybe Result -> (Result -> IO a) -> IO a
unlessError c msg mres m = do
  case mres of
    Just res -> do
      st <- resultStatus res
      case st of
        BadResponse       -> doError c msg
        FatalError        -> doError c msg
        NonfatalError     -> doError c msg
        _                 -> m res
    Nothing -> throwM $ DbError "unable to submit query to server"

doError :: Connection -> String -> IO a
doError c msg = do
  me <- errorMessage c
  throwM $ SqlError $ concat
    [ msg
    , maybe "" ((": " ++) . BS.unpack) me
    ]

-- | Custom column types for postgres.
pgColType :: PPConfig -> SqlTypeRep -> T.Text
pgColType _ TRowID    = "BIGSERIAL"
pgColType _ TInt      = "INT8"
pgColType _ TFloat    = "FLOAT8"
pgColType _ TDateTime = "TIMESTAMP"
pgColType _ TBlob     = "BYTEA"
pgColType cfg t       = ppType cfg t
#endif
