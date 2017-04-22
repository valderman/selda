{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, RankNTypes, AllowAmbiguousTypes #-}
module Database.Selda.Selectors where
import Database.Selda.Table
import Database.Selda.Types
import Database.Selda.Column
import Data.Dynamic
import Unsafe.Coerce

-- | The inductive tuple of selectors for a table of type @a@.
type family Selectors t a where
  Selectors t (a :*: b) = ((t -> a) :*: Selectors t b)
  Selectors t a         = (t -> a)

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
selectors :: forall s a. HasSelectors a a
          => Table a -> Selectors (Cols s a) (Cols s a)
selectors _ = unsafeCoerce $ mkSel (Proxy :: Proxy a) 0 (Proxy :: Proxy a)

-- | Any table type that can have selectors generated.
class Typeable t => HasSelectors t a where
  mkSel :: Proxy t -> Int -> Proxy a -> Selectors t a

instance (Typeable a, HasSelectors t b) => HasSelectors t (a :*: b) where
  mkSel p n _ = (sel :*: mkSel p n (Proxy :: Proxy b))
    where
      sel tup = fromDyn (toDyns tup !! n) (error "impossible")

instance {-# OVERLAPPABLE #-}
         (Typeable t, Typeable a, Selectors t a ~ (t -> a)) =>
         HasSelectors t a where
  mkSel _ n _ = \tup -> fromDyn (toDyns tup !! n) (error "impossible")

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
tableWithSelectors :: forall s a. (TableSpec a, HasSelectors a a)
                   => TableName
                   -> ColSpecs a
                   -> (Table a, Selectors (Cols s a) (Cols s a))
tableWithSelectors name cs = (t, unsafeCoerce s)
  where
    t = table name cs
    s = selectors t
