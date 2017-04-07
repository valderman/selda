{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
-- | API for executing queries and building backends.
module Database.Selda.Backend
  ( Result, Res, MonadIO (..), MonadTrans (..), MonadThrow (..), MonadCatch (..)
  , QueryRunner, SeldaT, Param (..), Lit (..), SqlValue (..), Proxy (..)
  , query, queryWith, runSeldaT
  ) where
import Database.Selda.Column
import Database.Selda.SQL (Param (..))
import Database.Selda.Query.Type
import Database.Selda.Compile
import Data.Proxy
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Text (Text)

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

-- | Build the final result from a list of result columns.
queryWith :: forall s m a. (MonadIO m, Result a)
          => QueryRunner -> Query s a -> m [Res a]
queryWith qr =
  liftIO . fmap (mkResults (Proxy :: Proxy a)) . uncurry qr . compile

-- | Generate the final result of a query from a list of untyped result rows.
mkResults :: Result a => Proxy a -> [[SqlValue]] -> [Res a]
mkResults p = map (toRes p)
