{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
-- | API for executing queries and building backends.
module Database.Selda.Backend
  ( -- * High-level API
    Result, Res, MonadIO (..), SeldaT
  , query, insert
  , createTable, tryCreateTable
  , dropTable, tryDropTable
    -- * Low-level API for creating backends and custom queries.
  , MonadTrans (..), MonadThrow (..), MonadCatch (..)
  , QueryRunner, Param (..), Lit (..), Proxy (..), SqlValue (..)
  , exec, queryWith, runSeldaT
  ) where
import Database.Selda.Compile
import Database.Selda.Query.Type
import Database.Selda.SQL (Param (..))
import Database.Selda.SqlType
import Database.Selda.Table
import Database.Selda.Table.Compile
import Data.Proxy
import Data.Text (Text)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader

-- | A function which executes a query and gives back a list of extensible
--   tuples; one tuple per result row, and one tuple element per column.
type QueryRunner = Text -> [Param] -> IO [[SqlValue]]

-- | Monad transformer adding Selda SQL capabilities.
newtype SeldaT m a = S {unS :: ReaderT QueryRunner m a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask, MonadTrans
           )

-- | Run a Selda transformer. Backends should use this to implement their
--   @withX@ functions.
runSeldaT :: SeldaT m a -> QueryRunner -> m a
runSeldaT m = runReaderT (unS m)

-- | Run a query within a Selda transformer.
--   Selda transformers are entered using backend-specific @withX@ functions,
--   such as 'withSQLite' from the SQLite backend.
query :: forall s m a. (MonadIO m, Result a) => Query s a -> SeldaT m [Res a]
query q = S $ do
  runner <- ask
  queryWith runner q

-- | Insert the given values into the given table. All fields of the table must
--   be present.
insert :: (MonadIO m, Insert a) => Table a -> a -> SeldaT m ()
insert t cs = uncurry exec $ compileInsert t cs

-- | Create a table from the given schema.
createTable :: MonadIO m => Table a -> SeldaT m ()
createTable = flip exec [] . compileCreateTable Fail

-- | Create a table from the given schema, unless it already exists.
tryCreateTable :: MonadIO m => Table a -> SeldaT m ()
tryCreateTable = flip exec [] . compileCreateTable Ignore

-- | Drop the given table.
dropTable :: MonadIO m => Table a -> SeldaT m ()
dropTable = flip exec [] . compileDropTable Fail

-- | Drop the given table, if it exists.
tryDropTable :: MonadIO m => Table a -> SeldaT m ()
tryDropTable = flip exec [] . compileDropTable Ignore

-- | Build the final result from a list of result columns.
queryWith :: forall s m a. (MonadIO m, Result a)
          => QueryRunner -> Query s a -> m [Res a]
queryWith qr =
  liftIO . fmap (mkResults (Proxy :: Proxy a)) . uncurry qr . compile

-- | Generate the final result of a query from a list of untyped result rows.
mkResults :: Result a => Proxy a -> [[SqlValue]] -> [Res a]
mkResults p = map (toRes p)

-- | Execute a statement without a result.
exec :: MonadIO m => Text -> [Param] -> SeldaT m ()
exec q ps = S $ do
  runner <- ask
  void . liftIO $ runner q ps
