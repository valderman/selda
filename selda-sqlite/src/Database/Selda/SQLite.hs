{-# LANGUAGE GADTs, CPP, OverloadedStrings #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite (withSQLite, sqliteOpen, seldaClose) where
import Database.Selda
import Database.Selda.Backend
import Data.Dynamic
import Data.Text (pack)
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
        liftIO $ runStmt backend "PRAGMA foreign_keys = ON;" []
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
  , ppConfig        = defPPConfig {ppMaxInsertParams = Just 999}
  , backendId       = SQLite
  , closeConnection = \conn -> do
      stmts <- allStmts conn
      flip mapM_ stmts $ \(_, stm) -> do
        finalize $ fromDyn stm (error "BUG: non-statement SQLite statement")
      close db
  }

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
