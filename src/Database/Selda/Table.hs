{-# LANGUAGE GADTs, TypeOperators, PolyKinds, KindSignatures #-}
-- | Selda table definition language.
module Database.Selda.Table where
import Data.Proxy

type ColName = String
type TableName = String

data a :*: b where
  (:*:) :: a -> b -> a :*: b
infixr 1 :*:

instance (Show a, Show b) => Show (a :*: b) where
  show (a :*: b) = show a ++ " :*: " ++ show b

-- | A database table.
--   Tables are parameterized over their column types. For instance, a table
--   containing one string and one integer, in that order, would have the type
--   @Table '[String, Int]@, and a table containing only a single string column
--   would have the type @Table '[String]@.
data Table (a :: k) = Table
  { tableName :: TableName
  , tableCols :: [(Qualifier, ColName)]
  }

(¤) :: Table a -> Table b -> Table (a :*: b)
a ¤ b = b {tableCols = tableCols a ++ tableCols b}
infixr 1 ¤

-- | Determines whether a column is primary key, required or nullable.
data Qualifier = Primary | Required | Nullable
  deriving (Show, Eq, Ord)

-- | A non-nullable column with the given name.
required :: ColName -> Table a
required name = Table "" [(Required, name)]

-- | A nullable column with the given name.
nullable :: ColName -> Table a
nullable name = Table "" [(Nullable, name)]

-- | Marks the given column as the table's primary key.
primary :: ColName -> Table a
primary name = Table "" [(Primary, name)]

-- | A table with the given name and columns.
table :: TableName -> Table a -> Table a
table name tbl = tbl {tableName = name}
