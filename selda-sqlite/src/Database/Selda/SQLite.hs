{-# LANGUAGE GADTs, CPP, OverloadedStrings #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite (withSQLite, sqliteOpen, seldaClose) where
import Database.Selda
import Database.Selda.Backend
import Data.Dynamic
import Data.Text as Text (pack, toLower, take, isSuffixOf)
import Control.Monad (void, when, unless)
import Control.Monad.Catch
#ifndef __HASTE__
import Database.SQLite3
import System.Directory (makeAbsolute)
#endif

-- | Open a new connection to an SQLite database.
--   The connection is reusable across calls to `runSeldaT`, and must be
--   explicitly closed using 'seldaClose' when no longer needed.
sqliteOpen :: (MonadIO m, MonadMask m) => FilePath -> m SeldaConnection
sqliteOpen file = do
#ifdef __HASTE__
  error "sqliteOpen called in JS context"
#else
  mask $ \restore -> do
    edb <- try $ liftIO $ open (pack file)
    case edb of
      Left e@(SQLError{}) -> do
        throwM (DbError (show e))
      Right db -> flip onException (liftIO (close db)) . restore $ do
        absFile <- liftIO $ pack <$> makeAbsolute file
        let backend = sqliteBackend db
        void . liftIO $ runStmt backend "PRAGMA foreign_keys = ON;" []
        newConnection backend absFile
#endif

-- | Perform the given computation over an SQLite database.
--   The database is guaranteed to be closed when the computation terminates.
withSQLite :: (MonadIO m, MonadMask m) => FilePath -> SeldaT m a -> m a
#ifdef __HASTE__
withSQLite _ _ = return $ error "withSQLite called in JS context"
#else
withSQLite file m = bracket (sqliteOpen file) seldaClose (runSeldaT m)

sqliteBackend :: Database -> SeldaBackend
sqliteBackend db = SeldaBackend
  { runStmt         = \q ps -> snd <$> sqliteQueryRunner db q ps
  , runStmtWithPK   = \q ps -> fst <$> sqliteQueryRunner db q ps
  , prepareStmt     = \_ _ -> sqlitePrepare db
  , runPrepared     = sqliteRunPrepared db
  , getTableInfo    = sqliteGetTableInfo db . fromTableName
  , ppConfig        = defPPConfig {ppMaxInsertParams = Just 999}
  , backendId       = SQLite
  , closeConnection = \conn -> do
      stmts <- allStmts conn
      flip mapM_ stmts $ \(_, stm) -> do
        finalize $ fromDyn stm (error "BUG: non-statement SQLite statement")
      close db
  , disableForeignKeys = disableFKs db
  }

sqliteGetTableInfo :: Database -> Text -> IO TableInfo
sqliteGetTableInfo db tbl = do
    cols <- (snd . snd) <$> sqliteQueryRunner db tblinfo []
    indexes <- (snd . snd) <$> sqliteQueryRunner db indexes []
    fks <- (snd . snd) <$> sqliteQueryRunner db fklist []
    indexes' <- mapM indexInfo indexes
    colInfos <- mapM (describe fks indexes') cols
    return $ TableInfo
      { tableColumnInfos = colInfos
      , tableUniqueGroups =
        [ map mkColName names
        | (names, "u") <- indexes'
        ]
      }
  where
    tblinfo = mconcat ["PRAGMA table_info(", tbl, ");"]
    indexes = mconcat ["PRAGMA index_list(", tbl, ");"]
    fklist = mconcat ["PRAGMA foreign_key_list(", tbl, ");"]
    ixinfo name = mconcat ["PRAGMA index_info(", name, ");"]

    toTypeRep _ "text"                      = Right TText
    toTypeRep _ "double"                    = Right TFloat
    toTypeRep _ "boolean"                   = Right TBool
    toTypeRep _ "datetime"                  = Right TDateTime
    toTypeRep _ "date"                      = Right TDate
    toTypeRep _ "time"                      = Right TTime
    toTypeRep _ "blob"                      = Right TBlob
    toTypeRep True "integer"                = Right TRowID
    toTypeRep pk s | Text.take 3 s == "int" = Right $ if pk then TRowID else TInt
    toTypeRep _ typ                         = Left typ

    indexInfo [_, SqlString ixname, _, SqlString itype, _] = do
      let q = ixinfo ixname
      info <- (snd . snd) <$> sqliteQueryRunner db q []
      return $ (map (\[_,_,SqlString name] -> name) info, itype)
    indexInfo _ = do
      error "unreachable"

    describe fks ixs [_, SqlString name, SqlString ty, SqlInt nonnull, _, SqlInt pk] = do
      return $ ColumnInfo
        { colName = mkColName name
        , colType = toTypeRep (pk == 1) (toLower ty)
        , colIsPK = pk == 1
        , colIsAutoIncrement = "auto_increment" `isSuffixOf` ty
        , colHasIndex = any (== ([name], "c")) ixs
        , colIsUnique = any (== ([name], "u")) ixs || pk == 1
        , colIsNullable = nonnull == 0
        , colFKs =
            [ (mkTableName reftbl, mkColName refkey)
            | (_:_:SqlString reftbl:SqlString key:SqlString refkey:_) <- fks
            , key == name
            ]
        }
    describe _ _ result = do
      throwM $ SqlError $ "bad result from PRAGMA table_info: " ++ show result

disableFKs :: Database -> Bool -> IO ()
disableFKs db disable = do
    unless disable $ void $ sqliteQueryRunner db "COMMIT;" []
    void $ sqliteQueryRunner db q []
    when disable $ void $ sqliteQueryRunner db "BEGIN TRANSACTION;" []
  where
    q | disable   = "PRAGMA foreign_keys = OFF;"
      | otherwise = "PRAGMA foreign_keys = ON;"

sqlitePrepare :: Database -> Text -> IO Dynamic
sqlitePrepare db qry = do
  eres <- try $ prepare db qry
  case eres of
    Left e@(SQLError{}) -> throwM (SqlError (show e))
    Right r             -> return $ toDyn r

sqliteRunPrepared :: Database -> Dynamic -> [Param] -> IO (Int, [[SqlValue]])
sqliteRunPrepared db hdl params = do
  eres <- try $ do
    let Just stm = fromDynamic hdl
    sqliteRunStmt db stm params `finally` do
      clearBindings stm
      reset stm
  case eres of
    Left e@(SQLError{}) -> throwM (SqlError (show e))
    Right res           -> return (snd res)

sqliteQueryRunner :: Database -> QueryRunner (Int, (Int, [[SqlValue]]))
sqliteQueryRunner db qry params = do
    eres <- try $ do
      stm <- prepare db qry
      sqliteRunStmt db stm params `finally` do
        finalize stm
    case eres of
      Left e@(SQLError{}) -> throwM (SqlError (show e))
      Right res           -> return res

sqliteRunStmt :: Database -> Statement -> [Param] -> IO (Int, (Int, [[SqlValue]]))
sqliteRunStmt db stm params = do
  bind stm [toSqlData p | Param p <- params]
  rows <- getRows stm []
  rid <- lastInsertRowId db
  cs <- changes db
  return (fromIntegral rid, (cs, [map fromSqlData r | r <- rows]))

getRows :: Statement -> [[SQLData]] -> IO [[SQLData]]
getRows s acc = do
  res <- step s
  case res of
    Row -> do
      cs <- columns s
      getRows s (cs : acc)
    _ -> do
      return $ reverse acc

toSqlData :: Lit a -> SQLData
toSqlData (LInt i)      = SQLInteger $ fromIntegral i
toSqlData (LDouble d)   = SQLFloat d
toSqlData (LText s)     = SQLText s
toSqlData (LDateTime s) = SQLText s
toSqlData (LDate s)     = SQLText s
toSqlData (LTime s)     = SQLText s
toSqlData (LBool b)     = SQLInteger $ if b then 1 else 0
toSqlData (LBlob b)     = SQLBlob b
toSqlData (LNull)       = SQLNull
toSqlData (LJust x)     = toSqlData x
toSqlData (LCustom l)   = toSqlData l

fromSqlData :: SQLData -> SqlValue
fromSqlData (SQLInteger i) = SqlInt $ fromIntegral i
fromSqlData (SQLFloat f)   = SqlFloat f
fromSqlData (SQLText s)    = SqlString s
fromSqlData (SQLBlob b)    = SqlBlob b
fromSqlData SQLNull        = SqlNull
#endif
