{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Basic Selda types.
module Database.Selda.Types
  ( (:*:)(..), Head, Append (..), (:++:), ToDyn, Tup (..)
  , first, second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth
  , ColName, TableName
  , mkColName, mkTableName, addColSuffix, addColPrefix
  , fromColName, fromTableName
  , toDyns, fromDyns, unsafeToList, unsafeFromList
  ) where
import Data.Dynamic
import Data.String
import Data.Text (Text, replace, append)
import GHC.Generics (Generic)
import Unsafe.Coerce

#ifndef NO_LOCALCACHE
import Data.Hashable

instance Hashable TableName where
  hashWithSalt s (TableName tn) = hashWithSalt s tn
#endif

-- | Name of a database column.
newtype ColName = ColName Text
  deriving (Ord, Eq, Show, IsString)

-- | Name of a database table.
newtype TableName = TableName Text
  deriving (Ord, Eq, Show, IsString)

-- | Add a prefix to a column name.
addColPrefix :: ColName -> Text -> ColName
addColPrefix (ColName cn) s = ColName $ Data.Text.append s cn

-- | Add a suffix to a column name.
addColSuffix :: ColName -> Text -> ColName
addColSuffix (ColName cn) s = ColName $ Data.Text.append cn s

-- | Convert a column name into a string, with quotes.
fromColName :: ColName -> Text
fromColName (ColName cn) = mconcat ["\"", escapeQuotes cn, "\""]

-- | Convert a table name into a string, with quotes.
fromTableName :: TableName -> Text
fromTableName (TableName tn) = mconcat ["\"", escapeQuotes tn, "\""]

-- | Create a column name.
mkColName :: Text -> ColName
mkColName = ColName

-- | Create a column name.
mkTableName :: Text -> TableName
mkTableName = TableName

-- | Escape double quotes in an SQL identifier.
escapeQuotes :: Text -> Text
escapeQuotes = Data.Text.replace "\"" "\"\""

-- | An inductively defined "tuple", or heterogeneous, non-empty list.
data a :*: b where
  (:*:) :: a -> b -> a :*: b
  deriving (Typeable, Generic)
infixr 1 :*:

instance (Show a, Show b) => Show (a :*: b) where
  show (a :*: b) = show a ++ " :*: " ++ show b

instance (Eq a, Eq b) => Eq (a :*: b) where
  (a :*: b) == (a' :*: b') = a == a' && b == b'

instance (Ord a, Ord b) => Ord (a :*: b) where
  (a :*: b) `compare` (a' :*: b') =
    case a `compare` a' of
      EQ -> b `compare` b'
      o  -> o

type family Head a where
  Head (a :*: b) = a
  Head a         = a

class Tup a where
  tupHead :: a -> Head a

instance {-# OVERLAPPING #-} Tup (a :*: b) where
  tupHead (a :*: _) = a

instance Head a ~ a => Tup a where
  tupHead a = a

-- | Get the first element of an inductive tuple.
first :: Tup a => a -> Head a
first = tupHead

-- | Get the second element of an inductive tuple.
second :: Tup b => (a :*: b) -> Head b
second (_ :*: b) = tupHead b

-- | Get the third element of an inductive tuple.
third :: Tup c => (a :*: b :*: c) -> Head c
third (_ :*: _ :*: c) = tupHead c

-- | Get the fourth element of an inductive tuple.
fourth :: Tup d => (a :*: b :*: c :*: d) -> Head d
fourth (_ :*: _ :*: _ :*: d) = tupHead d

-- | Get the fifth element of an inductive tuple.
fifth :: Tup e => (a :*: b :*: c :*: d :*: e) -> Head e
fifth (_ :*: _ :*: _ :*: _ :*: e) = tupHead e

-- | Get the sixth element of an inductive tuple.
sixth :: Tup f => (a :*: b :*: c :*: d :*: e :*: f) -> Head f
sixth (_ :*: _ :*: _ :*: _ :*: _ :*: f) = tupHead f

-- | Get the seventh element of an inductive tuple.
seventh :: Tup g => (a :*: b :*: c :*: d :*: e :*: f :*: g) -> Head g
seventh (_ :*: _ :*: _ :*: _ :*: _ :*: _ :*: g) = tupHead g

-- | Get the eighth element of an inductive tuple.
eighth :: Tup h => (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h) -> Head h
eighth (_ :*: _ :*: _ :*: _ :*: _ :*: _ :*: _ :*: h) = tupHead h

-- | Get the ninth element of an inductive tuple.
ninth :: Tup i => (a :*: b :*: c :*: d :*: e :*: f :*: h :*: h :*: i) -> Head i
ninth (_ :*: _ :*: _ :*: _ :*: _ :*: _ :*: _ :*: _ :*: i) = tupHead i

-- | Get the tenth element of an inductive tuple.
tenth :: Tup j => (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h :*: i :*: j)
      -> Head j
tenth (_ :*: _ :*: _ :*: _ :*: _ :*: _ :*: _ :*: _ :*: _ :*: j) = tupHead j

-- | Normalized append of two inductive tuples.
--   Note that this will flatten any nested inductive tuples.
type family a :++: b where
  (a :*: b) :++: c = a :*: (b :++: c)
  a         :++: b = a :*: b

class Append a b where
  app :: a -> b -> a :++: b
instance {-# OVERLAPPING #-} Append b c => Append (a :*: b) c where
  app (a :*: b) c = a :*: app b c
instance ((a :*: b) ~ (a :++: b)) => Append a b where
  app a b = a :*: b

data Unsafe = Unsafe Int

-- Prevent users from creating unsafe instances of ToDyn.
class ToDynInternal a => ToDyn a
instance ToDynInternal a => ToDyn a

class Typeable a => ToDynInternal a where
  toDyns :: a -> [Dynamic]
  fromDyns :: [Dynamic] -> Maybe a
  -- | TODO: replace with safe coercions when that hits platform-1.
  unsafeToList :: a -> [Unsafe]
  -- | TODO: replace with safe coercions when that hits platform-1.
  unsafeFromList :: [Unsafe] -> a

instance (Typeable a, ToDynInternal b) => ToDynInternal (a :*: b) where
  toDyns (a :*: b) = toDyn a : toDyns b
  fromDyns (x:xs) = do
    x' <- fromDynamic x
    xs' <- fromDyns xs
    return (x' :*: xs')
  fromDyns _ = do
    Nothing
  unsafeToList (x :*: xs) = unsafeCoerce x : unsafeToList xs
  unsafeFromList (x : xs) = unsafeCoerce x :*: unsafeFromList xs
  unsafeFromList _        = error "too short list to unsafeFromList"

instance {-# OVERLAPPABLE #-} Typeable a => ToDynInternal a where
  toDyns a = [toDyn a]
  fromDyns [x] = fromDynamic x
  fromDyns _   = Nothing
  unsafeToList x = [unsafeCoerce x]
  unsafeFromList [x] = unsafeCoerce x
  unsafeFromList []  = error "too short list to unsafeFromList"
  unsafeFromList _   = error "too long list to unsafeFromList"
