{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, ConstraintKinds, UndecidableSuperClasses #-}
{-# LANGUAGE TypeApplications, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Create Selda selectors from plain record field selectors.
--   Requires the @OverloadedLabels@ language extension.
module Database.Selda.FieldSelectors
  (FieldType, HasField, IsLabel
  ) where
import Database.Selda.Generic (Relational)
import Database.Selda.Selectors as S
import Database.Selda.SqlType (SqlType)
import Data.Kind (Constraint)
import GHC.Generics
import GHC.TypeLits
import GHC.OverloadedLabels

-- | Get the next nested type.
type family GetFieldType (f :: * -> *) :: * where
  GetFieldType (M1 c i f) = GetFieldType f
  GetFieldType (K1 i a)   = a

-- | Get the type of the field @name@ from the generic representation @a@,
--   returning the default value @b@ if the field does not exist.
type family GFieldType (a :: * -> *) (b :: *) (name :: Symbol) :: * where
  GFieldType (M1 S ('MetaSel ('Just name) su ss ds) f) b name = GetFieldType f
  GFieldType (M1 c i a) b name = GFieldType a b name
  GFieldType (a :*: b) c name  = GFieldType a (GFieldType b c name) name
  GFieldType a b name          = b

-- | The type of the @name@ field, in the record type @t@.
type FieldType name t = GFieldType (Rep t) (NoSuchSelector t name) name

type family NonError (t :: k) :: Constraint where
  NonError (NoSuchSelector t s) = TypeError
    ( 'Text "Row type '" ':<>: 'ShowType t ':<>:
      'Text "' has no selector " ':<>: 'ShowType s ':<>: 'Text "."
    )
  NonError t = ()

-- | Internal representation of the "no such selector" error message.
data NoSuchSelector (t :: *) (s :: Symbol)

-- | Any table type @t@, which has a field named @name@.
class ( Relational t
      , SqlType (FieldType name t)
      , GRSel name (Rep t)
      , NonError (FieldType name t)) =>
  HasField (name :: Symbol) t

instance ( Relational t
         , SqlType (FieldType name t)
         , GRSel name (Rep t)
         , NonError (FieldType name t)) =>
  HasField (name :: Symbol) t

instance (Relational t, HasField name t, FieldType name t ~ a) =>
         IsLabel name (S.Selector t a) where
#if MIN_VERSION_base(4, 10, 0)
  fromLabel = field @name @t
#else
  fromLabel _ = field @name @t
#endif

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
  case gSel @name @(Rep t) 0 of
    Left n -> unsafeSelector n
    _      -> error "unreachable"

class GRSel (s :: Symbol) (f :: * -> *) where
  gSel :: Int -> Either Int Int

instance GRSel name (M1 S ('MetaSel ('Just name) su ss ds) f) where
  gSel = Left

instance {-# OVERLAPPABLE #-} GRSel name f => GRSel name (M1 i s f) where
  gSel = gSel @name @f

instance (GRSel name a, GRSel name b) => GRSel name (a :*: b) where
  gSel n = gSel @name @a n >>= gSel @name @b . succ

instance GRSel name (K1 i a) where
  gSel = Right
