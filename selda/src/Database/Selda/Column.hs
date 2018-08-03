{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
-- | Columns and associated utility functions, specialized to 'SQL'.
module Database.Selda.Column
  ( Cols, Columns
  , Col (..), SomeCol (..), UntypedCol (..)
  , Exp (..), NulOp (..), UnOp (..), BinOp (..)
  , toTup, fromTup, liftC, liftC2, liftC3
  , allNamesIn
  , hideRenaming
  , literal
  ) where
import Database.Selda.Exp
import Database.Selda.SQL
import Database.Selda.SqlType
import Database.Selda.SqlResult
import Database.Selda.Types
import Data.Proxy
import Data.String
import Data.Text (Text)

-- | Convert a tuple of Haskell types to a tuple of column types.
type family Cols s a where
  Cols s (a :*: b)      = Col s a :*: Cols s b
  Cols s a              = Col s a

-- | Any column tuple.
class Columns a where
  toTup :: [ColName] -> a
  fromTup :: a -> [UntypedCol SQL]

instance (SqlResult a, Columns b) => Columns (Col s a :*: b) where
  toTup xs =
    case nestedCols (Proxy :: Proxy a) of
      0 -> One (Col (head xs)) :*: toTup (tail xs)
      n -> Many (map (Untyped . Col) (take n xs)) :*: toTup (drop n xs)
  fromTup (One x :*: xs) = Untyped x : fromTup xs
  fromTup (Many xs :*: xss) = xs ++ fromTup xss

instance Columns (Col s a) where
  toTup [x] = One (Col x)
  toTup []  = error "too few elements to toTup"
  toTup xs  = Many (map (Untyped . Col) xs)
  fromTup (One x) = [Untyped x]
  fromTup (Many xs) = xs

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
data Col s a where
  One :: !(Exp SQL a) -> Col s a
  Many :: ![UntypedCol SQL] -> Col s a

-- | A literal expression.
literal :: SqlType a => a -> Col s a
literal = One . Lit . mkLit

instance IsString (Col s Text) where
  fromString = literal . fromString

liftC3 :: (Exp SQL a -> Exp SQL b -> Exp SQL c -> Exp SQL d)
       -> Col s a
       -> Col s b
       -> Col s c
       -> Col s d
liftC3 f (One a) (One b) (One c) = One (f a b c)
liftC3 _ _ _ _ = error "Can't use liftC3 with product columns"

liftC2 :: (Exp SQL a -> Exp SQL b -> Exp SQL c) -> Col s a -> Col s b -> Col s c
liftC2 f (One a) (One b) = One (f a b)
liftC2 _ _ _ = error "Can't use liftC2 with product columns"

liftC :: (Exp SQL a -> Exp SQL b) -> Col s a -> Col s b
liftC f (One x) = One (f x)
liftC _ _ = error "Can't use liftC with product columns"

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
