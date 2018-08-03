{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, RankNTypes, AllowAmbiguousTypes, GADTs #-}
{-# LANGUAGE DeriveGeneric, CPP #-}
module Database.Selda.Selectors
  ( Assignment ((:=)), Selected, Selector, Source, Selectors, GSelectors
  , (!), with, ($=)
  , selectorsFor, selectorIndex
  ) where
import Control.Monad.State.Strict
import Database.Selda.SqlType
import Database.Selda.Types
import Database.Selda.Column
import Data.List (foldl')
import Data.Proxy
import GHC.Generics hiding (Selector, (:*:))
import qualified GHC.Generics as G
import Unsafe.Coerce

-- | The result of a '(!)' operation.
--   If either the source composite column, the value column to extract,
--   or both, is nullable, the result is also nullable.
type family Selected a b where
  Selected (Maybe a) (Maybe b) = Maybe b
  Selected (Maybe a) b         = Maybe b
  Selected a         b         = b

-- | The source type of a '(!)' operation.
type family Source a where
  Source (Maybe a) = a
  Source a         = a

-- | Extract the given value column from the given composite column.
--   Extracting a value from a nullable column will yield a nullable value.
--   In other words, this operator is null-coalescing.
(!) :: SqlType b => Col s a -> Selector (Source a) b -> Col s (Selected a b)
(Many xs) ! (Selector i) = unsafeCoerce (xs !! i)
(One _)   ! _            = nonProdColError

nonProdColError :: a
nonProdColError = error "BUG: used selector on non-product column"

upd :: Col s a -> Assignment s a -> Col s a
upd (Many xs) (Selector i := (One x')) =
  case splitAt i xs of
    (left, _:right) -> Many (left ++ Untyped x' : right)
    _               -> error "impossible"
upd (Many xs) (Modify (Selector i) f) =
  case splitAt i xs of
    (left, Untyped x:right) -> Many (left ++ f' (unsafeCoerce x) : right)
    _               -> error "impossible"
  where
    f' x = case f (One x) of
      One y -> Untyped y
      _     -> nonProdColError
upd _ _ =
  nonProdColError

-- | A selector-value assignment pair.
data Assignment s a where
#if MIN_VERSION_base(4, 9, 0)
  -- | Set the given column to the given value.
#endif
  (:=) :: Selector t a -> Col s a -> Assignment s t

#if MIN_VERSION_base(4, 9, 0)
  -- | Modify the given column by the given function.
#endif
  Modify :: Selector t a -> (Col s a -> Col s a) -> Assignment s t
infixl 2 :=

-- | Apply the given function to the given column.
($=) :: Selector t a -> (Col s a -> Col s a) -> Assignment s t
($=) = Modify
infixl 2 $=

-- | For each selector-value pair in the given list, on the given tuple,
--   update the field pointed out by the selector with the corresponding value.
with :: Col s a -> [Assignment s a] -> Col s a
with = foldl' upd

-- | A column selector. Column selectors can be used together with the '!' and
--   'with' functions to get and set values on composite columns, or to specify
--   foreign keys.
newtype Selector t a = Selector {selectorIndex :: Int}

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

instance SqlType a => GSelectors t (K1 i a) where
  mkSel _ _ = Selector <$> state (\n -> (n, n+1))

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
