{-# LANGUAGE GADTs, CPP, OverloadedStrings #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite (withSQLite) where
import Database.Selda
import Database.Selda.Backend
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
      runSeldaT m backend `finally` liftIO (close db)

sqliteBackend :: MVar () -> FilePath -> Database -> SeldaBackend
sqliteBackend lock dbfile db = SeldaBackend
  { runStmt        = \q ps -> snd <$> sqliteQueryRunner lock db q ps
  , runStmtWithPK  = \q ps -> fst <$> sqliteQueryRunner lock db q ps
  , ppConfig       = defPPConfig
  , dbIdentifier   = pack dbfile
  }

sqliteQueryRunner :: MVar () -> Database -> QueryRunner (Int, (Int, [[SqlValue]]))
sqliteQueryRunner lock db qry params = do
    eres <- try $ do
      stm <- prepare db qry
      takeMVar lock
      go stm `finally` do
        putMVar lock ()
        finalize stm
    case eres of
      Left e@(SQLError{}) -> throwM (SqlError (show e))
      Right res           -> return res
  where
    go stm = do
      bind stm [toSqlData p | Param p <- params]
      rows <- getRows stm []
      rid <- lastInsertRowId db
      cs <- changes db
      return (fromIntegral rid, (cs, [map fromSqlData r | r <- rows]))

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
toSqlData (LNull)       = SQLNull
toSqlData (LJust x)     = toSqlData x
toSqlData (LCustom l)   = toSqlData l

fromSqlData :: SQLData -> SqlValue
fromSqlData (SQLInteger i) = SqlInt $ fromIntegral i
fromSqlData (SQLFloat f)   = SqlFloat f
fromSqlData (SQLText s)    = SqlString s
fromSqlData (SQLBlob _)    = error "BUG: SQLite returned BLOB"
fromSqlData SQLNull        = SqlNull
#endif
