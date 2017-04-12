{-# LANGUAGE GADTs, CPP #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite (withSQLite) where
import Database.Selda
import Database.Selda.Backend
import Data.Text (pack)
import Control.Monad.Catch
import Control.Concurrent
#ifndef __HASTE__
import Database.SQLite3
#endif

-- | Perform the given computation over an SQLite database.
--   The database is guaranteed to be closed when the computation terminates.
withSQLite :: (MonadIO m, MonadMask m) => FilePath -> SeldaT m a -> m a
#ifdef __HASTE__
withSQLite _ _ = return $ error "withSQLite called in JS context"
#else
withSQLite file m = do
  lock <- liftIO $ newMVar ()
  db <- liftIO $ open (pack file)
  runSeldaT m (sqliteBackend lock db) `finally` liftIO (close db)

sqliteBackend :: MVar () -> Database -> SeldaBackend
sqliteBackend lock db = SeldaBackend
  { runStmt       = \q ps -> snd <$> sqliteQueryRunner lock db q ps
  , runStmtWithPK = \q ps -> fst <$> sqliteQueryRunner lock db q ps
  , customColType = \_ _ -> Nothing
  }

sqliteQueryRunner :: MVar () -> Database -> QueryRunner (Int, (Int, [[SqlValue]]))
sqliteQueryRunner lock db qry params = do
    stm <- prepare db qry
    takeMVar lock
    go stm `finally` do
      putMVar lock ()
      finalize stm
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
toSqlData (LitI i)    = SQLInteger $ fromIntegral i
toSqlData (LitD d)    = SQLFloat d
toSqlData (LitS s)    = SQLText s
toSqlData (LitTS s)   = SQLText s
toSqlData (LitDate s) = SQLText s
toSqlData (LitTime s) = SQLText s
toSqlData (LitB b)    = SQLInteger $ if b then 1 else 0
toSqlData (LitNull)   = SQLNull
toSqlData (LitJust x) = toSqlData x

fromSqlData :: SQLData -> SqlValue
fromSqlData (SQLInteger i) = SqlInt $ fromIntegral i
fromSqlData (SQLFloat f)   = SqlFloat f
fromSqlData (SQLText s)    = SqlString s
fromSqlData (SQLBlob _)    = error "Selda doesn't support blobs"
fromSqlData SQLNull        = SqlNull
#endif
