{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies, FlexibleInstances,
             ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
-- | API for executing queries and building backends.
module Database.Selda.Backend
  ( Result, Res, MonadIO (..), MonadTrans (..), MonadThrow (..), MonadCatch (..)
  , Query.Query, QueryRunner, SeldaT, Param (..), Lit (..), SqlValue (..)
  , query, Query.compile, runSeldaT, toRes
  ) where
import qualified Database.Selda.Query as Query
import Database.Selda.Column
import Database.Selda.Table
import Database.Selda.SQL (Param (..))
import Data.Proxy
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Monad.Reader

-- | A function which executes a query and gives back a list of extensible
--   tuples; one tuple per result row, and one tuple element per column.
type QueryRunner = forall a. Result a => Query.Query a -> IO [Res a]

-- Impredicative polymorphism workaround
newtype QR = QR {run :: QueryRunner}

-- | Monad transformer adding Selda SQL capabilities.
newtype SeldaT m a = S {unS :: ReaderT QR m a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask, MonadTrans
           )

runSeldaT :: SeldaT m a -> QueryRunner -> m a
runSeldaT m r = runReaderT (unS m) (QR r)

-- | Some value that is representable in SQL.
data SqlValue where
  SqlInt    :: Int    -> SqlValue
  SqlFloat  :: Double -> SqlValue
  SqlString :: String -> SqlValue
  SqlBool   :: Bool   -> SqlValue

class SqlData a where fromSql :: SqlValue -> a
instance SqlData Int    where fromSql (SqlInt x) = x
instance SqlData Double where fromSql (SqlFloat x) = x
instance SqlData String where fromSql (SqlString x) = x
instance SqlData Bool   where fromSql (SqlBool x) = x

-- | An acceptable query result type; one or more columns stitched together
--   with @:*:@.
class Query.Result r => Result r where
  type Res r
  -- | Converts the given list of @SqlValue@s into an tuple of well-typed
  --   results.
  --   See 'querySQLite' for example usage.
  --   The given list must contain exactly as many elements as dictated by
  --   the @Res r@. If the result is @a :*: b :*: c@, then the list must
  --   contain exactly three values, for instance.
  toRes :: Proxy r -> [SqlValue] -> Res r

instance (SqlData a, Result b) => Result (Col a :*: b) where
  type Res (Col a :*: b) = a :*: Res b
  toRes _ (x:xs) = fromSql x :*: toRes (Proxy :: Proxy b) xs

instance SqlData a => Result (Col a) where
  type Res (Col a) = a
  toRes _ [x] = fromSql x

-- | Run a query within a Selda transformer.
--   Selda transformers are entered using backend-specific @withX@ functions,
--   such as 'withSQLite' from the SQLite backend.
query :: (MonadIO m, Result a) => Query.Query a -> SeldaT m [Res a]
query q = S $ ask >>= \runner -> liftIO $ run runner q
