{-# LANGUAGE GADTs, TypeOperators #-}
-- | Basic Selda types.
module Database.Selda.Types where
import Data.Text (Text)

-- | Name of a database colmn.
type ColName = Text

-- | An extensible "tuple", or heterogeneous, non-empty list.
data a :*: b where
  (:*:) :: a -> b -> a :*: b
infixr 1 :*:

instance (Show a, Show b) => Show (a :*: b) where
  show (a :*: b) = show a ++ " :*: " ++ show b
