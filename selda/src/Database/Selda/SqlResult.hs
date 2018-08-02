{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, DefaultSignatures #-}
module Database.Selda.SqlResult
  ( SqlResult (..), ResultReader
  , runResultReader, next
  ) where
import Database.Selda.SqlType
import Control.Monad.State
import GHC.Generics

import Data.Typeable
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

newtype ResultReader a = R (State [SqlValue] a)
  deriving (Functor, Applicative, Monad)

runResultReader :: ResultReader a -> [SqlValue] -> a
runResultReader (R m) = evalState m

next :: ResultReader SqlValue
next = R . state $ \s -> (head s, tail s)

class Typeable a => SqlResult a where
  nextResult :: ResultReader a

  default nextResult :: (Generic a, GSqlResult (Rep a)) => ResultReader a
  nextResult = to <$> gNextResult

instance SqlResult RowID where
  nextResult = fromSql <$> next
instance Typeable a => SqlResult (ID a) where
  nextResult = fromSql <$> next
instance SqlResult Int where
  nextResult = fromSql <$> next
instance SqlResult Double where
  nextResult = fromSql <$> next
instance SqlResult Text where
  nextResult = fromSql <$> next
instance SqlResult Bool where
  nextResult = fromSql <$> next
instance SqlResult UTCTime where
  nextResult = fromSql <$> next
instance SqlResult Day where
  nextResult = fromSql <$> next
instance SqlResult TimeOfDay where
  nextResult = fromSql <$> next
instance SqlResult ByteString where
  nextResult = fromSql <$> next
instance SqlResult BSL.ByteString where
  nextResult = fromSql <$> next
instance SqlType a => SqlResult (Maybe a) where
  nextResult = fromSql <$> next

class GSqlResult f where
  gNextResult :: ResultReader (f x)

instance SqlResult a => GSqlResult (K1 i a) where
  gNextResult = K1 <$> nextResult

instance GSqlResult f => GSqlResult (M1 c i f) where
  gNextResult = M1 <$> gNextResult

instance (GSqlResult a, GSqlResult b) => GSqlResult (a :*: b) where
  gNextResult = liftM2 (:*:) gNextResult gNextResult
