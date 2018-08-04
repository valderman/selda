{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, DefaultSignatures, ScopedTypeVariables, CPP #-}
#if MIN_VERSION_base(4, 10, 0)
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
#endif
module Database.Selda.SqlRow
  ( SqlRow (..), ResultReader
  , runResultReader, next
  ) where
import Control.Monad.State.Strict
import Database.Selda.SqlType
import Data.Typeable
import GHC.Generics

newtype ResultReader a = R (State [SqlValue] a)
  deriving (Functor, Applicative, Monad)

runResultReader :: ResultReader a -> [SqlValue] -> a
runResultReader (R m) = evalState m

next :: ResultReader SqlValue
next = R . state $ \s -> (head s, tail s)

class Typeable a => SqlRow a where
  -- | Read the next, potentially composite, result from a stream of columns.
  nextResult :: ResultReader a
  default nextResult :: (Generic a, GSqlRow (Rep a)) => ResultReader a
  nextResult = to <$> gNextResult

  -- | The number of nested columns contained in this type.
  nestedCols :: Proxy a -> Int
  default nestedCols :: (Generic a, GSqlRow (Rep a)) => Proxy a -> Int
  nestedCols _ = gNestedCols (Proxy :: Proxy (Rep a))


-- * Generic derivation for SqlRow
class GSqlRow f where
  gNextResult :: ResultReader (f x)
  gNestedCols :: Proxy f -> Int

instance SqlType a => GSqlRow (K1 i a) where
  gNextResult = K1 <$> fromSql <$> next
  gNestedCols _ = 1

instance GSqlRow f => GSqlRow (M1 c i f) where
  gNextResult = M1 <$> gNextResult
  gNestedCols _ = gNestedCols (Proxy :: Proxy f)

instance (GSqlRow a, GSqlRow b) => GSqlRow (a :*: b) where
  gNextResult = liftM2 (:*:) gNextResult gNextResult
  gNestedCols _ = gNestedCols (Proxy :: Proxy a) + gNestedCols (Proxy :: Proxy b)


-- * Various instances
instance
  ( Typeable (a, b)
  , GSqlRow (Rep (a, b))
  ) => SqlRow (a, b)
instance
  ( Typeable (a, b, c)
  , GSqlRow (Rep (a, b, c))
  ) => SqlRow (a, b, c)
instance
  ( Typeable (a, b, c, d)
  , GSqlRow (Rep (a, b, c, d))
  ) => SqlRow (a, b, c, d)
instance
  ( Typeable (a, b, c, d, e)
  , GSqlRow (Rep (a, b, c, d, e))
  ) => SqlRow (a, b, c, d, e)
instance
  ( Typeable (a, b, c, d, e, f)
  , GSqlRow (Rep (a, b, c, d, e, f))
  ) => SqlRow (a, b, c, d, e, f)
instance
  ( Typeable (a, b, c, d, e, f, g)
  , GSqlRow (Rep (a, b, c, d, e, f, g))
  ) => SqlRow (a, b, c, d, e, f, g)
