{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Database.Selda.MySQL
  ( withMySQL, mySQLOpen, seldaClose
  -- * MySQL connection information.
  -- | Reexported from mysql-haskell.
  , ConnectInfo(..)) where
import Database.Selda.MySQL.Encoding
import Data.Dynamic
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding
import Database.Selda.Backend as S
import Control.Monad (void)
import Control.Monad.IO.Class
import Database.MySQL.Base as MySQL
import System.IO.Streams.List as IOStreams
import Database.MySQL.Connection
import Data.IORef
import Data.List
import Data.Function
import Control.Monad
import Control.Monad.Fix
import Control.Exception hiding (bracket, bracketOnError)
import Control.Monad.Catch (MonadThrow, MonadMask, bracket)
import Debug.Trace

-- | Perform the given computation over a MySQL database.  The
--   database connection is guaranteed to be closed when the
--   computation terminates.
withMySQL :: (MonadIO m, MonadThrow m, MonadMask m)
          => ConnectInfo -> SeldaT m a -> m a
withMySQL ci m = bracket (mySQLOpen ci) seldaClose (runSeldaT m)

-- | Open a new MySQL connection. The connection will persist across
--   calls to 'runSeldaT', and must be explicitly closed using 'seldaClose'
--   when no longer needed.
mySQLOpen :: (MonadIO m, MonadMask m) => ConnectInfo -> m SeldaConnection
mySQLOpen ci = do
  c <- liftIO $ connect ci
  liftIO $ ansiQuotes c
  newConnection (mySQLBackend c ci) (connString ci)

connString :: ConnectInfo -> T.Text
connString conn = mconcat
  [ "host=", T.pack $ ciHost conn, " "
  , "port=", T.pack $ show $ ciPort conn, " "
  , "dbname=", decodeUtf8 $ ciDatabase conn, " "
  , "user=" <>  decodeUtf8 (ciUser conn) <> " "
  , "password=" <> decodeUtf8 (ciPassword conn) <> " "
  , "connect_timeout=10", " "
  , "client_encoding=UTF8"
  ]
  
mySQLBackend :: MySQLConn -> ConnectInfo -> SeldaBackend
mySQLBackend c ci = SeldaBackend
  { runStmt            = mySQLRunStmt c 
  , runStmtWithPK      = mySQLRunStmtWithPK c
  , S.prepareStmt      = mySQLPrepare c
  , runPrepared        = mySQLQueryStmt c
  , getTableInfo       = mySQLGetTableInfo ci . rawTableName
  , backendId          = Other "MySQL"
  , ppConfig           = mySQLPPConfig
  , closeConnection    = \_ -> close c
  , disableForeignKeys = disableFKs c
  }

mySQLPrepare :: MySQLConn -> S.StmtID -> [SqlTypeRep] -> T.Text
             -> IO Dynamic
mySQLPrepare c _ _ q =
  fmap toDyn $ MySQL.prepareStmt c $
  Query $ LB.fromStrict $ encodeUtf8 q
  
mySQLGetTableInfo :: ConnectInfo -> T.Text -> IO TableInfo
mySQLGetTableInfo ci tbl = do
  conn <- connect $ ci { ciDatabase = "information_schema" }
  ansiQuotes conn
  columnFields <-
    IOStreams.toList =<<
    (snd <$> query conn
      (Query "SELECT column_name, is_nullable, data_type, column_key, extra \
             \FROM COLUMNS \
             \WHERE table_schema=? AND table_name=? \
             \ORDER BY ordinal_position; " )
      [MySQLText $ decodeUtf8 $ ciDatabase ci,
       MySQLText tbl])

  referencedColumns <-
    mapM (\colNam -> IOStreams.toList =<<
                     (snd <$> query conn
                      (Query "SELECT table_name, column_name \
                             \FROM KEY_COLUMN_USAGE WHERE \
                             \table_schema=? AND referenced_table_name=? \
                             \AND referenced_column_name = ?")
                      [ MySQLText $ decodeUtf8 $ ciDatabase ci
                      , MySQLText tbl
                      , colNam
                      ]))
    $ map head columnFields

  uniqueGroups <-
    fmap (filter (\e -> length e > 1) .
          fmap (\grp -> [mkColName col | [_, MySQLText col] <- grp]) .
          groupBy ((==) `on` head)) $
    IOStreams.toList =<<
    (snd <$> query conn
     (Query "SELECT index_name, column_name \
            \FROM information_schema.STATISTICS WHERE \
            \non_unique = 0 AND table_schema=? AND table_name=? \
            \ORDER BY index_name, seq_in_index;")
      [MySQLText $ decodeUtf8 $ ciDatabase ci,
       MySQLText tbl])

  let columnInfo = zipWith createColumnInfo columnFields referencedColumns
  
  pure $ TableInfo columnInfo uniqueGroups
  
createColumnInfo :: [MySQLValue] -> [[MySQLValue]] -> ColumnInfo
createColumnInfo [ MySQLText columnNam
                 , MySQLText isNullable
                 , MySQLText dataType
                 , MySQLText columnKey
                 , MySQLText extra]
                 referenceColumns =
  ColumnInfo
  { colName = mkColName columnNam
  , colType = toSqlTypeRep dataType
  , colIsPK = columnKey == "PRI"
  , colIsAutoIncrement = "auto_increment" `T.isInfixOf` extra
  , colIsUnique = columnKey == "UNI" || columnKey == "PRI"
  , colIsNullable = isNullable == "YES"
  , colHasIndex = columnKey == "PRI" ||
                  columnKey == "UNI" ||
                  columnKey == "MUL"
  , colFKs = [ (mkTableName tblName, mkColName colNam)
             | [MySQLText tblName, MySQLText colNam] <-
               referenceColumns
             ]
  }
    
createColumnInfo _ _ = error "BUG: column info returned wrong datatype"

mySQLRunStmtWithPK :: MySQLConn -> T.Text -> [S.Param] -> IO Int
mySQLRunStmtWithPK conn qry params =
  mySQLQuery conn qry params >>= \case
  Left ok -> pure (okLastInsertID ok)
  Right _ -> error "BUG: Statement returned results."

mySQLRunStmt :: MySQLConn -> T.Text -> [S.Param] -> IO (Int, [[SqlValue]])
mySQLRunStmt conn qry params =
  mySQLQuery conn qry params >>= \case
  Right vals -> pure (0, vals)
  Left ok -> pure (okAffectedRows ok, [])

mySQLQuery :: MySQLConn -> T.Text -> [S.Param] -> IO (Either OK [[SqlValue]])
mySQLQuery conn@(MySQLConn is os _ consumed) qryText params = do
  guardUnconsumed conn
  let Query qry =
        traceShow qryText $
        renderParams (Query $ LB.fromStrict $ encodeUtf8 qryText) $
                  [litToMySQL p | Param p <- params]
  writeCommand (COM_QUERY qry) os 
  p <- readPacket is
  if | isERR p -> decodeFromPacket p >>= throwIO . ERRException
     | isOK p -> Left <$> decodeFromPacket p
     | otherwise -> do
         len <- getFromPacket getLenEncInt p
         fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
         _ <- readPacket is -- eof packet, we don't verify this though
         writeIORef consumed False
         rows <- fix $ \loop -> do
           q <- readPacket is
           if  | isEOF q  -> writeIORef consumed True >> pure []
               | isERR q  -> decodeFromPacket q >>= throwIO . ERRException
               | otherwise -> liftM2 (:)
                              (getFromPacket (getTextRowSql fields) q)
                              loop
         pure $ Right rows

mySQLQueryStmt :: MySQLConn -> Dynamic -> [S.Param] -> IO (Int, [[SqlValue]])
mySQLQueryStmt conn@(MySQLConn is os _ consumed) stid params = do
  guardUnconsumed conn
  msqlStid <- pure $ fromDyn stid $ error "BUG: wrong type of stmtID"
  let mysqlValues = [litToMySQL lit | Param lit <- params]
  writeCommand (COM_STMT_EXECUTE msqlStid mysqlValues
                (makeNullMap mysqlValues)) os
  p <- readPacket is
  if | isERR p -> decodeFromPacket p >>= throwIO . ERRException
     | isOK p -> (,[]) . okAffectedRows <$> decodeFromPacket p
     | otherwise -> do
         len <- getFromPacket getLenEncInt p
         fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
         _ <- readPacket is -- eof packet, we don't verify this though
         writeIORef consumed False
         rows <- fix $ \loop -> do
           q <- readPacket is
           if  | isOK  q  -> liftM2 (:)
                             (getFromPacket (map mySQLToSql <$> 
                                             getBinaryRow fields len) q)
                             loop
               | isEOF q  -> writeIORef consumed True >> pure []
               | isERR q  -> decodeFromPacket q >>= throwIO . ERRException
               | otherwise -> throwIO (UnexpectedPacket q)
         pure (0, rows)

mySQLPPConfig :: PPConfig
mySQLPPConfig = PPConfig
    { ppType = fromSqlTypeRep
    , ppTypeHook = \ty _ _ -> fromSqlTypeRep ty
    , ppTypePK = fromSqlTypeRep
    , ppPlaceholder = const "?"
    , ppAutoIncInsert = "0"
    , ppColAttrs = T.unwords . map mySQLColAttr
    , ppColAttrsHook = \_ ats _ -> T.unwords $ map mySQLColAttr ats
    , ppIndexMethodHook = (" USING " <>) . compileIndexMethod
    , ppMaxInsertParams = Nothing
    }
  where
    compileIndexMethod BTreeIndex = "BTREE"
    compileIndexMethod HashIndex  = "HASH"

ansiQuotes :: MySQLConn -> IO ()
ansiQuotes conn = 
  void $ execute_ conn "SET sql_mode='ANSI_QUOTES';"
  

disableFKs :: MySQLConn -> Bool -> IO ()
disableFKs conn disable = 
  void $ execute_ conn $
  if disable then "SET FOREIGN_KEY_CHECKS=0;"
  else "SET FOREIGN_KEY_CHECKS=1;"

-- | Custom attribute types for postgres.
mySQLColAttr :: ColAttr -> T.Text
mySQLColAttr Primary       = "PRIMARY KEY"
mySQLColAttr AutoIncrement = "AUTO_INCREMENT"
mySQLColAttr Required      = "NOT NULL"
mySQLColAttr Optional      = ""
mySQLColAttr Unique        = "UNIQUE"
mySQLColAttr (Indexed _)   = ""

fromSqlTypeRep :: SqlTypeRep -> T.Text
fromSqlTypeRep TText = "VARCHAR"
fromSqlTypeRep TRowID = "BIGINT"
fromSqlTypeRep TInt = "BIGINT"
fromSqlTypeRep TFloat = "DOUBLE"
fromSqlTypeRep TBool = "TINYINT"
fromSqlTypeRep TDateTime = "DATETIME"
fromSqlTypeRep TDate = "DATE"
fromSqlTypeRep TTime = "TIME"
fromSqlTypeRep TBlob = "BLOB"

