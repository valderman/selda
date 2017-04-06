{-# LANGUAGE GADTs, ScopedTypeVariables #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite (withSQLite, querySQLite) where
import Database.Selda
import Database.Selda.Backend
import Database.SQLite3
import Data.Text (Text, pack, unpack)
import Data.Proxy
import Control.Monad.Catch

withSQLite :: (MonadIO m, MonadMask m) => FilePath -> SeldaT m a -> m a
withSQLite file m = do
  db <- liftIO $ open (pack file)
  runSeldaT m (querySQLite db) `finally` liftIO (close db)

querySQLite :: forall s a. Result a => Database -> Query s a -> IO [Res a]
querySQLite db q = do
    stm <- prepare db (pack query)
    bind stm [toSqlData p | Param p <- params]
    rows <- getRows stm []
    finalize stm
    return [toRes (Proxy :: Proxy a) (map fromSqlData r) | r <- rows]
  where
    (query, params) = compile q
    getRows s acc = do
      res <- step s
      case res of
        Row -> do
          cs <- columns s
          getRows s (cs : acc)
        _ -> do
          return $ reverse acc

toSqlData :: Lit a -> SQLData
toSqlData (LitI i) = SQLInteger $ fromIntegral i
toSqlData (LitD d) = SQLFloat d
toSqlData (LitS s) = SQLText $ pack s
toSqlData (LitB b) = SQLInteger $ if b then 1 else 0

fromSqlData :: SQLData -> SqlValue
fromSqlData (SQLInteger i) = SqlInt $ fromIntegral i
fromSqlData (SQLFloat f)   = SqlFloat f
fromSqlData (SQLText s)    = SqlString $ unpack s
