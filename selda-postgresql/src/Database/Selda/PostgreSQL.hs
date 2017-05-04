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
  , customColType  = pgColType
  , defaultKeyword = "DEFAULT"
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

    getLastId res = (read . BS.unpack . maybe "" id) <$> getvalue res 0 0

    getRows res = do
      rows <- ntuples res
      cols <- nfields res
      types <- mapM (ftype res) [0..cols-1]
      affected <- cmdTuples res
      result <- mapM (getRow res types cols) [0..rows-1]
      pure $ case affected of
        Just "" -> (0, result)
        Just s  -> (read $ BS.unpack s, result)
        _       -> (0, result)

    getRow res types cols row = do
      sequence $ zipWith (getCol res row) [0..cols-1] types

    getCol res row col t = do
      mval <- getvalue res row col
      case mval of
        Just val -> pure $ toSqlValue t val
        _        -> pure SqlNull

-- | Convert the given postgres return value and type to an @SqlValue@.
--   TODO: use binary format instead of text.
toSqlValue :: Oid -> BS.ByteString -> SqlValue
toSqlValue t val
  | t == boolType    = SqlBool $ readBool val
  | t == intType     = SqlInt $ read (BS.unpack val)
  | t == doubleType  = SqlFloat $ read (BS.unpack val)
  | t `elem` textish = SqlString (decodeUtf8 val)
  | otherwise        = error $ "BUG: result with unknown type oid: " ++ show t
  where
    textish = [textType, timestampType, timeType, dateType]
    readBool "f"   = False
    readBool "0"   = False
    readBool "0.0" = False
    readBool "F"   = False
    readBool _     = True

-- | Convert a parameter into an postgres parameter triple.
fromSqlValue :: Lit a -> Maybe (Oid, BS.ByteString, Format)
fromSqlValue (LitB b)    = Just (boolType, if b then "true" else "false", Text)
fromSqlValue (LitI n)    = Just (intType, BS.pack $ show n, Text)
fromSqlValue (LitD f)    = Just (doubleType, BS.pack $ show f, Text)
fromSqlValue (LitS s)    = Just (textType, encodeUtf8 s, Text)
fromSqlValue (LitTS s)   = Just (timestampType, encodeUtf8 s, Text)
fromSqlValue (LitTime s) = Just (timeType, encodeUtf8 s, Text)
fromSqlValue (LitDate s) = Just (dateType, encodeUtf8 s, Text)
fromSqlValue (LitNull)   = Nothing
fromSqlValue (LitJust x) = fromSqlValue x

-- | Custom column types for postgres: auto-incrementing primary keys need to
--   be @BIGSERIAL@, and ints need to be @INT8@.
pgColType :: T.Text -> [ColAttr] -> Maybe T.Text
pgColType "INTEGER" attrs
  | AutoIncrement `elem` attrs =
    Just "BIGSERIAL PRIMARY KEY NOT NULL"
  | otherwise =
    Just $ T.unwords ("INT8" : map compileColAttr attrs)
pgColType "DOUBLE" attrs =
    Just $ T.unwords ("FLOAT8" : map compileColAttr attrs)
pgColType "DATETIME" attrs =
    Just $ T.unwords ("TIMESTAMP" : map compileColAttr attrs)
pgColType _ _ =
    Nothing

-- | OIDs for all types used by Selda.
boolType, intType, textType, doubleType, dateType, timeType, timestampType :: Oid
boolType      = Oid 16
intType       = Oid 20
textType      = Oid 25
doubleType    = Oid 701
dateType      = Oid 1082
timeType      = Oid 1083
timestampType = Oid 1114
#endif
