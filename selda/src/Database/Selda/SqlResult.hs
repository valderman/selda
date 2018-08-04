{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, DefaultSignatures, ScopedTypeVariables, CPP #-}
#if MIN_VERSION_base(4, 10, 0)
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
#endif
module Database.Selda.SqlResult
  ( SqlResult (..), ResultReader
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

class Typeable a => SqlResult a where
  -- | Read the next, potentially composite, result from a stream of columns.
  nextResult :: ResultReader a
  default nextResult :: (Generic a, GSqlResult (Rep a)) => ResultReader a
  nextResult = to <$> gNextResult

  -- | The number of nested columns contained in this type.
  --   Must be 0 for instances of SqlType.
  nestedCols :: Proxy a -> Int
  default nestedCols :: (Generic a, GSqlResult (Rep a)) => Proxy a -> Int
  nestedCols _ = gNestedCols (Proxy :: Proxy (Rep a))


-- * Generic derivation for SqlResult
class GSqlResult f where
  gNextResult :: ResultReader (f x)
  gNestedCols :: Proxy f -> Int

instance SqlType a => GSqlResult (K1 i a) where
  gNextResult = K1 <$> fromSql <$> next
  gNestedCols _ = 1

instance GSqlResult f => GSqlResult (M1 c i f) where
  gNextResult = M1 <$> gNextResult
  gNestedCols _ = gNestedCols (Proxy :: Proxy f)

instance (GSqlResult a, GSqlResult b) => GSqlResult (a :*: b) where
  gNextResult = liftM2 (:*:) gNextResult gNextResult
  gNestedCols _ = gNestedCols (Proxy :: Proxy a) + gNestedCols (Proxy :: Proxy b)


-- * Various instances
instance {-# OVERLAPPABLE #-} (Typeable a, SqlType a) => SqlResult a where
  nextResult = fromSql <$> next
  nestedCols _ = 0

instance
  ( Typeable (a, b)
  , GSqlResult (Rep (a, b))
  ) => SqlResult (a, b)
instance
  ( Typeable (a, b, c)
  , GSqlResult (Rep (a, b, c))
  ) => SqlResult (a, b, c)
instance
  ( Typeable (a, b, c, d)
  , GSqlResult (Rep (a, b, c, d))
  ) => SqlResult (a, b, c, d)
instance
  ( Typeable (a, b, c, d, e)
  , GSqlResult (Rep (a, b, c, d, e))
  ) => SqlResult (a, b, c, d, e)
instance
  ( Typeable (a, b, c, d, e, f)
  , GSqlResult (Rep (a, b, c, d, e, f))
  ) => SqlResult (a, b, c, d, e, f)
instance
  ( Typeable (a, b, c, d, e, f, g)
  , GSqlResult (Rep (a, b, c, d, e, f, g))
  ) => SqlResult (a, b, c, d, e, f, g)
