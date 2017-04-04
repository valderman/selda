{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances #-}
-- | Columns and associated utility functions.
module Database.Selda.Column where
import Database.Selda.Table
import Data.String
import Data.Int

type family Cols a where
  Cols (a :*: b) = Col a :*: Cols b
  Cols a         = Col a

class Columns a where
  toTup :: [ColName] -> a

instance Columns b => Columns (Col a :*: b) where
  toTup (x:xs) = Col x :*: toTup xs

instance Columns (Col a) where
  toTup [x] = Col x

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol where
  Some  :: Col a -> SomeCol
  Named :: ColName -> Col a -> SomeCol

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
data Col a where
  Col   :: ColName -> Col a
  Lit   :: Lit a -> Col a
  BinOp :: BinOp a b -> Col a -> Col a -> Col b
  UnOp  :: UnOp a b -> Col a -> Col b

-- | Get all column names in the given expression.
allNamesIn :: Col a -> [ColName]
allNamesIn (Col n)       = [n]
allNamesIn (Lit _)       = []
allNamesIn (BinOp _ a b) = allNamesIn a ++ allNamesIn b

data UnOp a b where
  Abs :: UnOp a a
  Not :: UnOp Bool Bool
  Neg :: UnOp a a
  Sgn :: UnOp a a

data BinOp a b where
  Gt   :: BinOp a Bool
  Lt   :: BinOp a Bool
  Gte  :: BinOp a Bool
  Lte  :: BinOp a Bool
  Eq   :: BinOp a Bool
  Add  :: BinOp a a
  Sub  :: BinOp a a
  Mul  :: BinOp a a
  Div  :: BinOp a a
  Like :: BinOp String Bool

data Lit a where
  LitS :: String -> Lit String
  LitI :: Int    -> Lit Int
  LitD :: Double -> Lit Double
  LitB :: Bool   -> Lit Bool

instance Show (Lit a) where
  show (LitS s) = show s
  show (LitI i) = show i
  show (LitD d) = show d
  show (LitB b) = show b

class SqlType a where
  literal :: a -> Col a

instance SqlType Int where
  literal = Lit . LitI
instance SqlType Double where
  literal = Lit . LitD
instance SqlType String where
  literal = Lit . LitS
instance SqlType Bool where
  literal = Lit . LitB

instance IsString (Col String) where
  fromString = literal

instance (SqlType a, Num a) => Num (Col a) where
  fromInteger = literal . fromInteger
  (+) = BinOp Add
  (-) = BinOp Sub
  (*) = BinOp Mul
  negate = UnOp Neg
  abs = UnOp Abs
  signum = UnOp Sgn

instance Fractional (Col Double) where
  fromRational = literal . fromRational
  (/) = BinOp Div  

instance Fractional (Col Int) where
  fromRational = literal . truncate . fromRational
  (/) = BinOp Div
