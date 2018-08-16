{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, ConstraintKinds, UndecidableSuperClasses #-}
-- | Create Selda selectors from plain record field selectors.
--   Requires the @TypeApplications@ and @DataKinds@ language extensions
--   to be even remotely useful.
module Database.Selda.FieldSelectors
  ( FieldType, HasField
  , field
  ) where
import Database.Selda.Generic (Relational)
import Database.Selda.Selectors as S
import Data.Proxy
import Data.Kind (Constraint)
import GHC.Generics
import GHC.TypeLits

-- | Get the next nested type.
type family NextConType (f :: * -> *) :: * where
  NextConType (M1 c i f) = NextConType f
  NextConType (K1 i a)   = a

type family SelType' (a :: * -> *) (b :: *) (name :: Symbol) :: * where
  SelType' (M1 S ('MetaSel ('Just name) su ss ds) f) b name = NextConType f
  SelType' (M1 c i a) b name = SelType' a b name
  SelType' (a :*: b) c name  = SelType' a (SelType' b c name) name
  SelType' a b name          = b

-- | The type of the @name@ field, in the record type @t@.
type family FieldType (name :: Symbol) (t :: *) :: * where
  FieldType name t = SelType' (Rep t) (NoSuchSelector t name) name

type family NonError (t :: k) :: Constraint where
  NonError (NoSuchSelector t s) = TypeError
    ( 'Text "Row type '" ':<>: 'ShowType t ':<>:
      'Text "' has no selector " ':<>: 'ShowType s ':<>: 'Text "."
    )
  NonError t                 = ()

-- | Internal representation of the "no such selector" error message.
data NoSuchSelector (t :: *) (s :: Symbol)

-- | Any table type @t@, which has a field named @name@.
class (GRSel name (Rep t), NonError (FieldType name t)) =>
  HasField (name :: Symbol) t

instance (GRSel name (Rep t), NonError (FieldType name t)) =>
  HasField (name :: Symbol) t

-- | Create a selector from a record selector and a type application.
--
--   For example:
-- > data Foo = Foo
-- >   { foo :: Int
-- >   , bar :: Text
-- >   } deriving Generic
-- > instance SqlRow Foo
-- >
-- > fooTable :: Table Foo
-- > fooTable = table "foo"
-- >
-- > getAllBars :: Query s (Col s Text)
-- > getAllBars = do
-- >   t <- select fooTable
-- >   return (t ! field @"bar")
field :: forall name t.
       (Relational t, HasField name t)
    => S.Selector t (FieldType name t)
field =
  case gSel (Proxy :: Proxy (Rep t)) (Proxy :: Proxy name) 0 of
    Left n -> unsafeSelector n
    _      -> error "unreachable"

class GRSel (s :: Symbol) (f :: * -> *) where
  gSel :: Proxy f -> Proxy s -> Int -> Either Int Int

instance GRSel name (M1 S ('MetaSel ('Just name) su ss ds) f) where
  gSel _ _ n = Left n

instance {-# OVERLAPPABLE #-} GRSel name f => GRSel name (M1 i s f) where
  gSel _ = gSel (Proxy :: Proxy f)

instance (GRSel name a, GRSel name b) => GRSel name (a :*: b) where
  gSel _ p n = gSel (Proxy :: Proxy a) p n >>= gSel (Proxy :: Proxy b) p . succ

instance GRSel name (K1 i a) where
  gSel _ _ n = pure n
