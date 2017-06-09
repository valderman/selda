{-# LANGUAGE GADTs, CPP, OverloadedStrings #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite (withSQLite) where
import Database.Selda
import Database.Selda.Backend
import Data.Dynamic
import Data.Text (pack)
import Control.Monad.Catch
import Control.Concurrent
#ifndef __HASTE__
import Database.SQLite3
import System.Directory (makeAbsolute)
#endif

-- | Perform the given computation over an SQLite database.
--   The database is guaranteed to be closed when the computation terminates.
withSQLite :: (MonadIO m, MonadMask m) => FilePath -> SeldaT m a -> m a
#ifdef __HASTE__
withSQLite _ _ = return $ error "withSQLite called in JS context"
#else
withSQLite file m = do
  lock <- liftIO $ newMVar ()
  edb <- try $ liftIO $ open (pack file)
  case edb of
    Left e@(SQLError{}) -> do
      throwM (DbError (show e))
    Right db -> do
      absFile <- liftIO $ makeAbsolute file
      let backend = sqliteBackend lock absFile db
      liftIO $ runStmt backend "PRAGMA foreign_keys = ON;" []
      conn <- newConnection backend
      runSeldaT m conn `finally` do
        liftIO $ do
          stmts <- allStmts conn
          flip mapM_ stmts $ \(_, stm) -> do
            finalize $ fromDyn stm (error "BUG: non-statement SQLite statement")
          close db

sqliteBackend :: MVar () -> FilePath -> Database -> SeldaBackend
sqliteBackend lock dbfile db = SeldaBackend
  { runStmt        = \q ps -> snd <$> sqliteQueryRunner lock db q ps
  , runStmtWithPK  = \q ps -> fst <$> sqliteQueryRunner lock db q ps
  , prepareStmt    = \_ _ -> sqlitePrepare db
  , runPrepared    = sqliteRunPrepared lock db
  , ppConfig       = defPPConfig
  , dbIdentifier   = pack dbfile
  }

sqlitePrepare :: Database -> Text -> IO Dynamic
sqlitePrepare db qry = do
  eres <- try $ prepare db qry
  case eres of
    Left e@(SQLError{}) -> throwM (SqlError (show e))
    Right r             -> return $ toDyn r

sqliteRunPrepared :: MVar () -> Database -> Dynamic -> [Param] -> IO (Int, [[SqlValue]])
sqliteRunPrepared lock db hdl params = do
  eres <- try $ do
    takeMVar lock
    let Just stm = fromDynamic hdl
    sqliteRunStmt db stm params `finally` do
      putMVar lock ()
      clearBindings stm
      reset stm
  case eres of
    Left e@(SQLError{}) -> throwM (SqlError (show e))
    Right res           -> return (snd res)

sqliteQueryRunner :: MVar () -> Database -> QueryRunner (Int, (Int, [[SqlValue]]))
sqliteQueryRunner lock db qry params = do
    eres <- try $ do
      stm <- prepare db qry
      takeMVar lock
      sqliteRunStmt db stm params `finally` do
        putMVar lock ()
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
