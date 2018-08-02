{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, RankNTypes, AllowAmbiguousTypes, GADTs, CPP #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.Selda.Selectors
  ( Assignment (..), Selector, Selectors, GSelectors
  , (!), with
  , selectorsFor, selectorIndex
  ) where
import Control.Monad.State.Strict
import Database.Selda.SqlType
import Database.Selda.Types
import Database.Selda.Column
import Data.List (foldl')
#if MIN_VERSION_base(4, 10, 0)
import Data.Proxy
#endif
import GHC.Generics hiding (Selector, (:*:))
import qualified GHC.Generics as G
import Unsafe.Coerce

(!) :: Col s a -> Selector a b -> Col s b
(Many xs) ! (Selector i) = unsafeCoerce (xs !! i)
(One _)   ! _            = nonProdColError

nonProdColError :: a
nonProdColError = error "BUG: used selector on non-product column"

upd :: Col s a -> Assignment s a -> Col s a
upd (Many xs) (Selector i := x') =
  case splitAt i xs of
    (left, _:right) -> Many (left ++ unsafeCoerce x' : right)
    _               -> error "impossible"
upd (One _) _ =
  nonProdColError

-- | A selector-value assignment pair.
data Assignment s a where
  (:=) :: Selector t a -> Col s a -> Assignment s t
infixl 2 :=

-- | For each selector-value pair in the given list, on the given tuple,
--   update the field pointed out by the selector with the corresponding value.
with :: Col s a -> [Assignment s a] -> Col s a
with = foldl' upd

-- | A column selector. Column selectors can be used together with the '!' and
--   'with' functions to get and set values on inductive tuples, or to indicate
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
