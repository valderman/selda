{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies, FlexibleInstances,
             ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
-- | API for executing queries and building backends.
module Database.Selda.Backend
  ( Result, Res, MonadIO (..), MonadTrans (..), MonadThrow (..), MonadCatch (..)
  , QueryRunner, SeldaT, Param (..), Lit (..), SqlValue (..)
  , query, runSeldaT, toRes, finalCols
  ) where
import Database.Selda.Column
import Database.Selda.Table
import Database.Selda.SQL (Param (..))
import Database.Selda.Query.Type
import Data.Proxy
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Text (Text)

-- | A function which executes a query and gives back a list of extensible
--   tuples; one tuple per result row, and one tuple element per column.
type QueryRunner = forall s a. Result a => Query s a -> IO [Res a]

-- | Impredicative polymorphism workaround; not for public consumption.
newtype QR = QR {run :: QueryRunner}

-- | Monad transformer adding Selda SQL capabilities.
newtype SeldaT m a = S {unS :: ReaderT QR m a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask, MonadTrans
           )

-- | Run a Selda transformer. Backends should use this to implement their
--   @withX@ functions.
runSeldaT :: SeldaT m a -> QueryRunner -> m a
runSeldaT m r = runReaderT (unS m) (QR r)

-- | Some value that is representable in SQL.
data SqlValue where
  SqlInt    :: Int    -> SqlValue
  SqlFloat  :: Double -> SqlValue
  SqlString :: Text   -> SqlValue
  SqlBool   :: Bool   -> SqlValue

-- | Any SQL result data type.
class SqlData a where fromSql :: SqlValue -> a
instance SqlData Int    where fromSql (SqlInt x) = x
instance SqlData Double where fromSql (SqlFloat x) = x
instance SqlData Text   where fromSql (SqlString x) = x
instance SqlData Bool   where fromSql (SqlBool x) = x

-- | An acceptable query result type; one or more columns stitched together
--   with @:*:@.
class Result r where
  type Res r
  -- | Converts the given list of @SqlValue@s into an tuple of well-typed
  --   results.
  --   See 'querySQLite' for example usage.
  --   The given list must contain exactly as many elements as dictated by
  --   the @Res r@. If the result is @a :*: b :*: c@, then the list must
  --   contain exactly three values, for instance.
  toRes :: Proxy r -> [SqlValue] -> Res r

  -- | Produce a list of all columns present in the result.
  finalCols :: r -> [SomeCol]

instance (SqlData a, Result b) => Result (Col s a :*: b) where
  type Res (Col s a :*: b) = a :*: Res b
  toRes _ (x:xs) = fromSql x :*: toRes (Proxy :: Proxy b) xs
  finalCols (a :*: b) = finalCols a ++ finalCols b

instance SqlData a => Result (Col s a) where
  type Res (Col s a) = a
  toRes _ [x] = fromSql x
  finalCols (C c) = [Some c]

-- | Run a query within a Selda transformer.
--   Selda transformers are entered using backend-specific @withX@ functions,
--   such as 'withSQLite' from the SQLite backend.
query :: (MonadIO m, Result a) => Query s a -> SeldaT m [Res a]
query q = S $ ask >>= \runner -> liftIO $ run runner q
