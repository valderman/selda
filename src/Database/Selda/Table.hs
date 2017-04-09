{-# LANGUAGE GADTs, TypeOperators, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables #-}
-- | Selda table definition language.
module Database.Selda.Table where
import Database.Selda.Types
import Database.Selda.SqlType
import Data.Text (Text, unpack, intercalate)
import Data.Proxy
import Data.List (sort, group)
import Data.Monoid

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

-- | Insertion over all non-autoincrementing required columns.
--   Autoincrementing primary keys are automatically assigned their value.
type family InsertCols a where
  InsertCols (Auto a :*: b)  = InsertCols b
  InsertCols (a :*: Auto b)  = a
  InsertCols (a :*: b)       = a :*: InsertCols b
  InsertCols a               = a

-- | A database table.
--   Tables are parameterized over their column types. For instance, a table
--   containing one string and one integer, in that order, would have the type
--   @Table (Text :*: Int)@, and a table containing only a single string column
--   would have the type @Table Text@.
data Table a = Table
  { -- | Name of the table. NOT guaranteed to be a valid SQL name.
    tableName :: TableName
    -- | All table columns.
    --   Invariant: the 'colAttrs' list of each column is sorted and contains
    --   no duplicates.
  , tableCols :: [ColInfo]
  }

data ColInfo = ColInfo
  { colName  :: ColName
  , colType  :: Text
  , colAttrs :: [ColAttr]
  }

newCol :: forall a. SqlType a => ColName -> ColSpec a
newCol name = ColSpec [ColInfo
  { colName  = name
  , colType  = sqlType (Proxy :: Proxy a)
  , colAttrs = []
  }]

-- | A table column specification.
newtype ColSpec a = ColSpec [ColInfo]

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

-- | Indicates an automatically incrementing column.
--   Auto columns are usually not touched in @INSERT@ queries.
data Auto a

-- | Used by 'IsNullable' to indicate a nullable type.
data Nullable

-- | Used by 'IsNullable' to indicate a nullable type.
data NotNullable

-- | Is the given type nullable?
type family IsNullable a where
  IsNullable (Maybe a) = Nullable
  IsNullable a         = NotNullable

-- | Any SQL type which is NOT nullable.
class SqlType a => NonNull a
instance (SqlType a, IsNullable a ~ NotNullable) => NonNull a

-- | Column attributes such as nullability, auto increment, etc.
--   When adding elements, make sure that they are added in the order
--   required by SQL syntax, as this list is only sorted before being
--   pretty-printed.
data ColAttr = Primary | AutoIncrement | Required | Optional
  deriving (Show, Eq, Ord)

-- | A non-nullable column with the given name.
required :: NonNull a => ColName -> ColSpec a
required = addAttr Required . newCol

-- | A nullable column with the given name.
optional :: SqlType a => ColName -> ColSpec (Maybe a)
optional = addAttr Optional . newCol

-- | Marks the given column as the table's primary key.
--   A table may only have one primary key; marking more than one key as
--   primary will result in a run-time error.
primary :: NonNull a => ColName -> ColSpec a
primary = addAttr Primary . required

-- | Automatically increment the given attribute if not specified during insert.
--   Also adds the @PRIMARY KEY@ attribute on the column.
autoPrimary :: ColName -> ColSpec (Auto Int)
autoPrimary n = ColSpec [c {colAttrs = [Primary, AutoIncrement, Required]}]
  where ColSpec [c] = newCol n :: ColSpec Int

-- | Add an attribute to a column. Not for public consumption.
addAttr :: SqlType a => ColAttr -> ColSpec a -> ColSpec a
addAttr attr (ColSpec [ci]) = ColSpec [ci {colAttrs = attr : colAttrs ci}]
addAttr _ _                 = error "impossible: SqlType ColSpec with several columns"

-- | A table with the given name and columns.
table :: TableName -> ColSpec a -> Table a
table name (ColSpec cs) = Table
  { tableName = name
  , tableCols = validate name $ map tidy cs
  }

-- | Remove duplicate attributes.
tidy :: ColInfo -> ColInfo
tidy ci = ci {colAttrs = snub $ colAttrs ci}

-- | Sort a list and remove all duplicates from it.
snub :: (Ord a, Eq a) => [a] -> [a]
snub = map head . soup

-- | Sort a list, then group all identical elements.
soup :: Ord a => [a] -> [[a]]
soup = group . sort

-- | Ensure that there are no duplicate column names or primary keys.
validate :: TableName -> [ColInfo] -> [ColInfo]
validate name cis
  | null errs = cis
  | otherwise = error $ concat
      [ "validation of table ", unpack name, " failed:"
      , "\n  "
      , unpack $ intercalate "\n  " errs
      ]
  where
    errs = concat
      [ dupes
      , pkDupes
      , optionalRequiredMutex
      ]
    dupes =
      ["duplicate column: " <> x | (x:_:_) <- soup $ map colName cis]
    pkDupes =
      ["multiple primary keys" | (Primary:_:_) <- soup $ concatMap colAttrs cis]

    -- This should be impossible, but...
    optionalRequiredMutex =
      [ "BUG: column " <> colName ci <> " is both optional and required"
      | ci <- cis
      , Optional `elem` colAttrs ci && Required `elem` colAttrs ci
      ]
