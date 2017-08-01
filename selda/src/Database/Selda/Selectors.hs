{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, RankNTypes, AllowAmbiguousTypes, GADTs #-}
module Database.Selda.Selectors where
import Database.Selda.Table
import Database.Selda.Types
import Database.Selda.Column
import Data.Dynamic
import Data.List (foldl')
import Data.Proxy
import Unsafe.Coerce

-- | Get the value at the given index from the given inductive tuple.
(!)  :: forall s t a. ToDyn (Cols () t) => Cols s t -> Selector t a -> Col s a
tup ! (Selector n) = unsafeCoerce (unsafeToList (toU tup) !! n)
  where toU = unsafeCoerce :: Cols s t -> Cols () t

upd :: forall s t. (ToDyn (Cols () t))
     => Cols s t -> Assignment s t -> Cols s t
upd tup (Selector n := x) =
    fromU . unsafeFromList $ replace (unsafeToList $ toU tup) (unsafeCoerce x)
  where
    toU = unsafeCoerce :: Cols s t -> Cols () t
    fromU = unsafeCoerce :: Cols () t -> Cols s t
    replace xs x' =
      case splitAt n xs of
        (left, _:right) -> left ++ x' : right
        _               -> error "impossible"

-- | A selector-value assignment pair.
data Assignment s t where
  (:=) :: Selector t a -> Col s a -> Assignment s t
infixl 2 :=

-- | For each selector-value pair in the given list, on the given tuple,
--   update the field pointed out by the selector with the corresponding value.
with :: forall s t. (ToDyn (Cols () t))
     => Cols s t -> [Assignment s t] -> Cols s t
with = foldl' upd

-- | A column selector. Column selectors can be used together with the '!' and
--   'with' functions to get and set values on inductive tuples, or to indicate
--   foreign keys.
data Selector t a = Selector Int

-- | The inductive tuple of selectors for a table of type @a@.
type family Selectors t a where
  Selectors t (a :*: b) = (Selector t a :*: Selectors t b)
  Selectors t a         = Selector t a

-- | Generate selector functions for the given table.
--   Selectors can be used to access the fields of a query result tuple, avoiding
--   the need to pattern match on the entire tuple.
--
-- > tbl :: Table (Int :*: Text)
-- > tbl = table "foo" $ required "bar" :*: required "baz"
-- > (tblBar :*: tblBaz) = selectors tbl
-- >
-- > q :: Query s Text
-- > q = tblBaz <$> select tbl
selectors :: forall a. HasSelectors a a => Table a -> Selectors a a
selectors _ = mkSel (Proxy :: Proxy a) 0 (Proxy :: Proxy a)

-- | Any table type that can have selectors generated.
class HasSelectors t a where
  mkSel :: Proxy t -> Int -> Proxy a -> Selectors t a

instance (Typeable a, HasSelectors t b) => HasSelectors t (a :*: b) where
  mkSel p n _ = (Selector n :*: mkSel p (n+1) (Proxy :: Proxy b))

instance {-# OVERLAPPABLE #-} (Selectors t a ~ Selector t a) =>
         HasSelectors t a where
  mkSel _ n _ = Selector n

-- | A pair of the table with the given name and columns, and all its selectors.
--   For example:
--
-- > tbl :: Table (Int :*: Text)
-- > (tbl, tblBar :*: tblBaz)
-- >   =  tableWithSelectors "foo"
-- >   $  required "bar"
-- >   :*: required "baz"
-- >
-- > q :: Query s Text
-- > q = tblBaz <$> select tbl
tableWithSelectors :: forall a. (TableSpec a, HasSelectors a a)
                   => TableName
                   -> ColSpecs a
                   -> (Table a, Selectors a a)
tableWithSelectors name cs = (t, s)
  where
    t = table name cs
    s = selectors t
