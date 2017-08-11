{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances #-}
-- | Columns and associated utility functions, specialized to 'SQL'.
module Database.Selda.Column
  ( Cols, Columns
  , Col (..), SomeCol (..), Exp (..), UnOp (..), BinOp (..)
  , toTup, fromTup, liftC, liftC2, liftC3
  , allNamesIn
  , literal
  ) where
import Database.Selda.Exp
import Database.Selda.SQL
import Database.Selda.SqlType
import Database.Selda.Types
import Data.String
import Data.Text (Text)

-- | Convert a tuple of Haskell types to a tuple of column types.
type family Cols s a where
  Cols s (a :*: b)      = Col s a :*: Cols s b
  Cols s a              = Col s a

-- | Any column tuple.
class Columns a where
  toTup :: [ColName] -> a
  fromTup :: a -> [SomeCol SQL]

instance Columns b => Columns (Col s a :*: b) where
  toTup (x:xs) = C (Col x) :*: toTup xs
  toTup _      = error "too few elements to toTup"
  fromTup (C x :*: xs) = Some x : fromTup xs

instance Columns (Col s a) where
  toTup [x] = C (Col x)
  toTup []  = error "too few elements to toTup"
  toTup xs  = C (TblCol xs)
  fromTup (C x) = [Some x]

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
newtype Col s a = C {unC :: Exp SQL a}

-- | A literal expression.
literal :: SqlType a => a -> Col s a
literal = C . Lit . mkLit

instance IsString (Col s Text) where
  fromString = literal . fromString

liftC3 :: (Exp SQL a -> Exp SQL b -> Exp SQL c -> Exp SQL d) -> Col s a -> Col s b -> Col s c -> Col s d
liftC3 f (C a) (C b) (C c) = C (f a b c)

liftC2 :: (Exp SQL a -> Exp SQL b -> Exp SQL c) -> Col s a -> Col s b -> Col s c
liftC2 f (C a) (C b) = C (f a b)

liftC :: (Exp SQL a -> Exp SQL b) -> Col s a -> Col s b
liftC f = C . f . unC

instance (SqlType a, Num a) => Num (Col s a) where
  fromInteger = literal . fromInteger
  (+) = liftC2 $ BinOp Add
  (-) = liftC2 $ BinOp Sub
  (*) = liftC2 $ BinOp Mul
  negate = liftC $ UnOp Neg
  abs = liftC $ UnOp Abs
  signum = liftC $ UnOp Sgn

instance {-# OVERLAPPING #-} (SqlType a, Num a) => Num (Col s (Maybe a)) where
  fromInteger = literal . Just . fromInteger
  (+) = liftC2 $ BinOp Add
  (-) = liftC2 $ BinOp Sub
  (*) = liftC2 $ BinOp Mul
  negate = liftC $ UnOp Neg
  abs = liftC $ UnOp Abs
  signum = liftC $ UnOp Sgn

instance Fractional (Col s Double) where
  fromRational = literal . fromRational
  (/) = liftC2 $ BinOp Div  

instance Fractional (Col s (Maybe Double)) where
  fromRational = literal . Just . fromRational
  (/) = liftC2 $ BinOp Div  

instance Fractional (Col s Int) where
  fromRational = literal . (truncate :: Double -> Int) . fromRational
  (/) = liftC2 $ BinOp Div

instance Fractional (Col s (Maybe Int)) where
  fromRational = literal . Just . (truncate :: Double -> Int) . fromRational
  (/) = liftC2 $ BinOp Div
