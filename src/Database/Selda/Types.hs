{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, FlexibleInstances #-}
-- | Basic Selda types.
module Database.Selda.Types where
import Data.Text (Text)

-- | Name of a database column.
type ColName = Text

-- | Name of a database table.
type TableName = Text

-- | An extensible "tuple", or heterogeneous, non-empty list.
data a :*: b where
  (:*:) :: a -> b -> a :*: b
infixr 1 :*:

instance (Show a, Show b) => Show (a :*: b) where
  show (a :*: b) = show a ++ " :*: " ++ show b
