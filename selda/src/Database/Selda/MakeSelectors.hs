{-# LANGUAGE ScopedTypeVariables, TypeOperators, KindSignatures #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-- | Utilities for creating selectors for non-record types.
--   In general, you should really use record types for your tables and
--   their record labels (i.e. #label) as selectors using
--   the @OverloadedLabels@ extension instead.
module Database.Selda.MakeSelectors
 ( Selectors, GSelectors
 , selectors, tableWithSelectors
 ) where
import Control.Monad.State.Strict
import Data.Proxy
import GHC.Generics hiding (Selector, (:*:))
import qualified GHC.Generics as G
import Database.Selda.Generic (Relational)
import Database.Selda.Selectors
import Database.Selda.SqlRow
import Database.Selda.SqlType
import Database.Selda.Table
import Database.Selda.Types

-- | Generate selector functions for the given table.
--   Selectors can be used to access the fields of a query result tuple, avoiding
--   the need to pattern match on the entire tuple.
--
-- > tbl :: Table (Int, Text)
-- > tbl = table "foo" []
-- > (tblBar :*: tblBaz) = selectors tbl
-- >
-- > q :: Query s Text
-- > q = do
-- >   row <- select tbl
-- >   return (row ! tblBaz)
selectors :: forall a. (Relational a, GSelectors a (Rep a))
          => Table a
          -> Selectors a
selectors _ = selectorsFor (Proxy :: Proxy a)

-- | A pair of the table with the given name and columns, and all its selectors.
--   For example:
--
-- > tbl :: Table (Int, Text)
-- > (tbl, tblBar :*: tblBaz)
-- >   =  tableWithSelectors "foo" []
-- >
-- > q :: Query s Text
-- > q = tblBaz `from` select tbl
tableWithSelectors :: forall a. (Relational a, GSelectors a (Rep a))
                   => TableName
                   -> [Attr a]
                   -> (Table a, Selectors a)
tableWithSelectors name cs = (t, s)
  where
    t = table name cs
    s = selectors t

-- | Generate selectors for the given type.
selectorsFor :: forall r. GSelectors r (Rep r) => Proxy r -> Selectors r
selectorsFor = flip evalState 0 . mkSel (Proxy :: Proxy (Rep r))

-- | An inductive tuple of selectors for the given relation.
type Selectors r = Sels r (Rep r)

type family Sels t f where
  Sels t ((a G.:*: b) G.:*: c) = Sels t (a G.:*: (b G.:*: c))
  Sels t (a G.:*: b)           = Sels t a :*: Sels t b
  Sels t (M1 x y f)            = Sels t f
  Sels t (K1 i a)              = Selector t a

-- | Any table type that can have selectors generated.
class GSelectors t (f :: * -> *) where
  mkSel :: Proxy f -> Proxy t -> State Int (Sels t f)

instance (SqlRow t, SqlType a) => GSelectors t (K1 i a) where
  mkSel _ _ = unsafeSelector <$> state (\n -> (n, n+1))

instance (GSelectors t f, Sels t f ~ Sels t (M1 x y f)) =>
         GSelectors t (M1 x y f) where
  mkSel _ = mkSel (Proxy :: Proxy f)

instance GSelectors t (a G.:*: (b G.:*: c)) =>
         GSelectors t ((a G.:*: b) G.:*: c) where
  mkSel _ = mkSel (Proxy :: Proxy (a G.:*: (b G.:*: c)))

instance {-# OVERLAPPABLE #-}
  ( GSelectors t a
  , GSelectors t b
  , Sels t (a G.:*: b) ~ (Sels t a :*: Sels t b)
  ) => GSelectors t (a G.:*: b) where
    mkSel _ p = do
      x <- mkSel (Proxy :: Proxy a) p
      xs <- mkSel (Proxy :: Proxy b) p
      return (x :*: xs)
