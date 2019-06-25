{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, UndecidableInstances, MultiParamTypeClasses #-}
-- | Columns and associated utility functions, specialized to 'SQL'.
module Database.Selda.Column
  ( Columns, Same
  , Row (..), Col (..), SomeCol (..), UntypedCol (..)
  , Exp (..), NulOp (..), UnOp (..), BinOp (..)
  , toTup, fromTup, liftC, liftC2, liftC3
  , allNamesIn
  , hideRenaming
  , literal
  ) where
import Database.Selda.Exp
import Database.Selda.SQL
import Database.Selda.SqlType
import Database.Selda.SqlRow
import Database.Selda.Types
import Data.Proxy
import Data.String
import Data.Text (Text)
import GHC.TypeLits as TL

-- | Any column tuple.
class Columns a where
  toTup :: [ColName] -> a
  fromTup :: a -> [UntypedCol SQL]

instance (SqlType a, Columns b) => Columns (Col s a :*: b) where
  toTup (x:xs) = One (Col x) :*: toTup xs
  toTup []     = error "too few elements to toTup"
  fromTup (One x :*: xs) = Untyped x : fromTup xs

instance (SqlRow a, Columns b) => Columns (Row s a :*: b) where
  toTup xs =
    case nestedCols (Proxy :: Proxy a) of
      n -> Many (map (Untyped . Col) (take n xs)) :*: toTup (drop n xs)
  fromTup (Many xs :*: xss) = xs ++ fromTup xss

instance Columns (Col s a) where
  toTup [x] = One (Col x)
  toTup []  = error "too few elements to toTup"
  toTup _   = error "too many elements to toTup"
  fromTup (One x) = [Untyped x]

instance Columns (Row s a) where
  toTup xs = Many (map (Untyped . Col) xs)
  fromTup (Many xs) = xs

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
newtype Col s a = One (Exp SQL a)

-- | A database row. A row is a collection of one or more columns.
newtype Row s a = Many [UntypedCol SQL]

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

-- | Denotes that scopes @s@ and @t@ are identical.
class s ~ t => Same s t where
  liftC2 :: (Exp SQL a -> Exp SQL b -> Exp SQL c) -> Col s a -> Col t b -> Col s c
  liftC2 f (One a) (One b) = One (f a b)

instance {-# OVERLAPPING #-} Same s s
instance {-# OVERLAPPABLE #-} (s ~ t, TypeError
  ('TL.Text "An identifier from an outer scope may not be used in an inner query."))
  => Same s t

liftC :: (Exp SQL a -> Exp SQL b) -> Col s a -> Col s b
liftC f (One x) = One (f x)

instance (SqlType a, Num a) => Num (Col s a) where
  fromInteger = literal . fromInteger
  (+) = liftC2 $ BinOp Add
  (-) = liftC2 $ BinOp Sub
  (*) = liftC2 $ BinOp Mul
  negate = liftC $ UnOp Neg
  abs = liftC $ UnOp Abs
  signum = liftC $ UnOp Sgn

instance Fractional (Col s Double) where
  fromRational = literal . fromRational
  (/) = liftC2 $ BinOp Div

instance Fractional (Col s Int) where
  fromRational = literal . (truncate :: Double -> Int) . fromRational
  (/) = liftC2 $ BinOp Div
