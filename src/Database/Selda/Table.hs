{-# LANGUAGE GADTs, TypeOperators, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
-- | Selda table definition language.
module Database.Selda.Table where
import Data.Text (Text)

type ColName = Text
type TableName = Text

data a :*: b where
  (:*:) :: a -> b -> a :*: b
infixr 1 :*:

instance (Show a, Show b) => Show (a :*: b) where
  show (a :*: b) = show a ++ " :*: " ++ show b

type family a :+++: b where
  (a :*: b) :+++: c = a :*: (b :+++: c)
  a :+++: b         = a :*: b
infixr 5 :+++:
infixr 5 +++

class ComposeSpec t a b where
  -- | Combine the given tables or column specifications into a new
  --   column specification which can be used to create a new table.
  --   Useful for building composable table specifications.
  --
  --   Note that this function is only suitable for combining specifications
  --   which have a concrete type. To build a column specification from scratch,
  --   use '(¤)' instead.
  (+++) :: t a -> t b -> ColSpec (a :+++: b)

instance (ComposeSpec Table a b, ComposeSpec Table b c) =>
         ComposeSpec Table (a :*: b) c where
  a +++ b = ColSpec $ tableCols a ++ tableCols b

instance {-# OVERLAPPABLE #-} ((a :+++: b) ~ (a :*: b)) =>
         ComposeSpec Table a b where
  a +++ b = ColSpec $ tableCols a ++ tableCols b

instance (ComposeSpec ColSpec a b, ComposeSpec ColSpec b c) =>
         ComposeSpec ColSpec (a :*: b) c where
  ColSpec a +++ ColSpec b = ColSpec $ a ++ b

instance {-# OVERLAPPABLE #-} ((a :+++: b) ~ (a :*: b)) =>
         ComposeSpec ColSpec a b where
  ColSpec a +++ ColSpec b = ColSpec $ a ++ b

-- | A database table.
--   Tables are parameterized over their column types. For instance, a table
--   containing one string and one integer, in that order, would have the type
--   @Table (Text :*: Int)@, and a table containing only a single string column
--   would have the type @Table Text@.
data Table a = Table
  { tableName :: TableName
  , tableCols :: [(Qualifier, ColName)]
  }

-- | A table column specification.
newtype ColSpec a = ColSpec [(Qualifier, ColName)]

-- | Combine two column specifications.
--   Table descriptions are built by chaining columns using this operator:
--
-- > people :: Table (Text :*: Int :*: Maybe Text)
-- > people = table "people" $ required "name" ¤ required "age" ¤ optional "pet"
--
--   To combine two pre-built tables into a table comprised of both tables'
--   fields, see '(+++)'.
(¤) :: ColSpec a -> ColSpec b -> ColSpec (a :*: b)
ColSpec a ¤ ColSpec b = ColSpec (a ++ b)
infixr 1 ¤

-- | Determines whether a column is primary key, required or nullable.
data Qualifier = Primary | Required | Nullable
  deriving (Show, Eq, Ord)

-- | A non-nullable column with the given name.
required :: ColName -> ColSpec a
required name = ColSpec [(Required, name)]

-- | A nullable column with the given name.
optional :: ColName -> ColSpec (Maybe a)
optional name = ColSpec [(Nullable, name)]

-- | Marks the given column as the table's primary key.
primary :: ColName -> ColSpec a
primary name = ColSpec [(Primary, name)]

-- | A table with the given name and columns.
table :: TableName -> ColSpec a -> Table a
table name (ColSpec cs) = Table {tableName = name, tableCols = cs}
