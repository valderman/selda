{-# LANGUAGE GADTs #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite
  ( Database
  , open, close
  , withSQLite, querySQLite
  ) where
import Database.Selda
import Database.Selda.Backend
import Database.SQLite3
import Data.Text (pack)
import Control.Monad.Catch

-- | Perform the given computation over an SQLite database.
--   The database is guaranteed to be closed when the computation terminates.
withSQLite :: (MonadIO m, MonadMask m) => FilePath -> SeldaT m a -> m a
withSQLite file m = do
  db <- liftIO $ open (pack file)
  runSeldaT m (sqliteQueryRunner db) `finally` liftIO (close db)

sqliteQueryRunner :: Database -> QueryRunner
sqliteQueryRunner db qry params = do
    stm <- prepare db qry
    bind stm [toSqlData p | Param p <- params]
    rows <- getRows stm []
    finalize stm
    return [map fromSqlData r | r <- rows]
  where
    getRows s acc = do
      res <- step s
      case res of
        Row -> do
          cs <- columns s
          getRows s (cs : acc)
        _ -> do
          return $ reverse acc

-- | Perform an SQLite query with a previously opened database.
querySQLite :: Result a => Database -> Query s a -> IO [Res a]
querySQLite db = queryWith (sqliteQueryRunner db)

toSqlData :: Lit a -> SQLData
toSqlData (LitI i) = SQLInteger $ fromIntegral i
toSqlData (LitD d) = SQLFloat d
toSqlData (LitS s) = SQLText s
toSqlData (LitB b) = SQLInteger $ if b then 1 else 0

fromSqlData :: SQLData -> SqlValue
fromSqlData (SQLInteger i) = SqlInt $ fromIntegral i
fromSqlData (SQLFloat f)   = SqlFloat f
fromSqlData (SQLText s)    = SqlString s
fromSqlData (SQLBlob _)    = error "Selda doesn't support blobs"
fromSqlData SQLNull        = SqlNull
