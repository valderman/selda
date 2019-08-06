{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Basic Selda types.
module Database.Selda.Types
  ( (:*:)(..), Head, Tup (..)
  , first, second, third, fourth, fifth
  , ColName, TableName
  , modColName, mkColName, mkTableName, addColSuffix, addColPrefix
  , fromColName, fromTableName, rawTableName, intercalateColNames
  ) where
import Data.Dynamic
import Data.String
import Data.Text (Text, replace, append, intercalate)
import GHC.Generics (Generic)

-- | Name of a database column.
newtype ColName = ColName { unColName :: Text }
  deriving (Ord, Eq, Show, IsString)

-- | Name of a database table.
newtype TableName = TableName Text
  deriving (Ord, Eq, Show, IsString)

-- | Modify the given column name using the given function.
modColName :: ColName -> (Text -> Text) -> ColName
modColName (ColName cn) f = ColName (f cn)

-- | Add a prefix to a column name.
addColPrefix :: ColName -> Text -> ColName
addColPrefix (ColName cn) s = ColName $ Data.Text.append s cn

-- | Add a suffix to a column name.
addColSuffix :: ColName -> Text -> ColName
addColSuffix (ColName cn) s = ColName $ Data.Text.append cn s

-- | Convert a column name into a string, with quotes.
fromColName :: ColName -> Text
fromColName (ColName cn) = mconcat ["\"", escapeQuotes cn, "\""]

-- | Convert column names into a string, without quotes, intercalating the given
-- string.
--
-- @
-- intercalateColNames "_" [ColName "a", ColName "b"] == "a_b"
-- @
intercalateColNames :: Text -> [ColName] -> Text
intercalateColNames inter cs = intercalate inter (escapeQuotes . unColName <$> cs)

-- | Convert a table name into a string, with quotes.
fromTableName :: TableName -> Text
fromTableName (TableName tn) = mconcat ["\"", escapeQuotes tn, "\""]

-- | Convert a table name into a string, without quotes.
rawTableName :: TableName -> Text
rawTableName (TableName tn) = escapeQuotes tn

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
