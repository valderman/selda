{-# LANGUAGE TypeFamilies, GADTs, CPP #-}
module Database.Selda.Selectors
  ( Assignment ((:=)), Selector, Coalesce
  , (!), (?), with, ($=)
  , selectorIndex, unsafeSelector
  ) where
import Database.Selda.SqlRow (SqlRow)
import Database.Selda.SqlType
import Database.Selda.Column
import Data.List (foldl')
import Unsafe.Coerce

-- | Coalesce nested nullable column into a single level of nesting.
type family Coalesce a where
  Coalesce (Maybe (Maybe a)) = Coalesce (Maybe a)
  Coalesce a                 = a

-- | A selector indicating the nth (zero-based) column of a table.
--
--   Will cause errors in queries during compilation, execution, or both,
--   unless handled with extreme care. You really shouldn't use it at all.
unsafeSelector :: (SqlRow a, SqlType b) => Int -> Selector a b
unsafeSelector = Selector

-- | Extract the given column from the given row.
(!) :: SqlType a => Row s t -> Selector t a -> Col s a
(Many xs) ! (Selector i) = case xs !! i of Untyped x -> One (unsafeCoerce x)
infixl 9 !

-- | Extract the given column from the given nullable row.
--   Nullable rows usually result from left joins.
--   If a nullable column is extracted from a nullable row, the resulting
--   nested @Maybe@s will be squashed into a single level of nesting.
(?) :: SqlType a => Row s (Maybe t) -> Selector t a -> Col s (Coalesce (Maybe a))
(Many xs) ? (Selector i) = case xs !! i of Untyped x -> One (unsafeCoerce x)
infixl 9 ?

upd :: Row s a -> Assignment s a -> Row s a
upd (Many xs) (Selector i := (One x')) =
  case splitAt i xs of
    (left, _:right) -> Many (left ++ Untyped x' : right)
    _               -> error "BUG: too few columns in row!"
upd (Many xs) (Modify (Selector i) f) =
  case splitAt i xs of
    (left, Untyped x:right) -> Many (left ++ f' (unsafeCoerce x) : right)
    _                       -> error "BUG: too few columns in row!"
  where
    f' x = case f (One x) of
      One y -> Untyped y

-- | A selector-value assignment pair.
data Assignment s a where
  -- | Set the given column to the given value.
  (:=) :: Selector t a -> Col s a -> Assignment s t

  -- | Modify the given column by the given function.
  Modify :: Selector t a -> (Col s a -> Col s a) -> Assignment s t
infixl 2 :=

-- | Apply the given function to the given column.
($=) :: Selector t a -> (Col s a -> Col s a) -> Assignment s t
($=) = Modify
infixl 2 $=

-- | For each selector-value pair in the given list, on the given tuple,
--   update the field pointed out by the selector with the corresponding value.
with :: Row s a -> [Assignment s a] -> Row s a
with = foldl' upd

-- | A column selector. Column selectors can be used together with the '!' and
--   'with' functions to get and set values on rows, or to specify
--   foreign keys.
newtype Selector t a = Selector {selectorIndex :: Int}
