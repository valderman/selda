{-# LANGUAGE TypeOperators, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Selda table definition language.
module Database.Selda.Table where
import Database.Selda.Types
import Database.Selda.SqlType
import Control.Exception
import Data.Dynamic
import Data.List (sort, group)
import Data.Monoid
import Data.Proxy
import Data.Text (unpack, intercalate, any)

-- | An error occurred when validating a database table.
--   If this error is thrown, there is a bug in your database schema, and the
--   particular table that triggered the error is unusable.
--   Since validation is deterministic, this error will be thrown on every
--   consecutive operation over the offending table.
--
--   Therefore, it is not meaningful to handle this exception in any way,
--   just fix your bug instead.
data ValidationError = ValidationError String
  deriving (Show, Eq, Typeable)
instance Exception ValidationError

-- | A database table.
--   Tables are parameterized over their column types. For instance, a table
--   containing one string and one integer, in that order, would have the type
--   @Table (Text :*: Int)@, and a table containing only a single string column
--   would have the type @Table Text@.
--
--   Table and column names may contain any character except @\NUL@, and be
--   non-empty. Column names must be unique per table.
data Table a = Table
  { -- | Name of the table. NOT guaranteed to be a valid SQL name.
    tableName :: !TableName
    -- | All table columns.
    --   Invariant: the 'colAttrs' list of each column is sorted and contains
    --   no duplicates.
  , tableCols :: ![ColInfo]
    -- | Does the given table have an auto-incrementing primary key?
  , tableHasAutoPK :: !Bool
  }

data ColInfo = ColInfo
  { colName  :: !ColName
  , colType  :: !SqlTypeRep
  , colAttrs :: ![ColAttr]
  , colFKs   :: ![(Table (), ColName)]
  }

newCol :: forall a. SqlType a => ColName -> ColSpec a
newCol name = ColSpec [ColInfo
  { colName  = name
  , colType  = sqlType (Proxy :: Proxy a)
  , colAttrs = []
  , colFKs   = []
  }]

-- | A table column specification.
newtype ColSpec a = ColSpec {unCS :: [ColInfo]}

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
data ColAttr = Primary | AutoIncrement | Required | Optional | Unique
  deriving (Show, Eq, Ord)

-- | A non-nullable column with the given name.
required :: NonNull a => ColName -> ColSpec a
required = addAttr Required . newCol

-- | A nullable column with the given name.
optional :: NonNull a => ColName -> ColSpec (Maybe a)
optional = addAttr Optional . newCol

-- | Marks the given column as the table's primary key.
--   A table may only have one primary key; marking more than one key as
--   primary will result in 'ValidationError' during validation.
primary :: NonNull a => ColName -> ColSpec a
primary = addAttr Primary . unique . required

-- | Automatically increment the given attribute if not specified during insert.
--   Also adds the @PRIMARY KEY@ and @UNIQUE@ attributes on the column.
autoPrimary :: ColName -> ColSpec RowID
autoPrimary n = ColSpec [c {colAttrs = [Primary, AutoIncrement, Required, Unique]}]
  where ColSpec [c] = newCol n :: ColSpec RowID

-- | Add a uniqueness constraint to the given column.
--   Adding a uniqueness constraint to a column that is already implied to be
--   unique, such as a primary key, is a no-op.
unique :: SqlType a => ColSpec a -> ColSpec a
unique = addAttr Unique

-- | Add an attribute to a column. Not for public consumption.
addAttr :: SqlType a => ColAttr -> ColSpec a -> ColSpec a
addAttr attr (ColSpec [ci]) = ColSpec [ci {colAttrs = attr : colAttrs ci}]
addAttr _ _                 = error "impossible: ColSpec with several columns"

-- | An inductive tuple where each element is a column specification.
type family ColSpecs a where
  ColSpecs (a :*: b) = ColSpec a :*: ColSpecs b
  ColSpecs a         = ColSpec a

-- | An inductive tuple forming a table specification.
class TableSpec a where
  mergeSpecs :: Proxy a -> ColSpecs a -> [ColInfo]
instance TableSpec b => TableSpec (a :*: b) where
  mergeSpecs _ (ColSpec a :*: b) = a ++ mergeSpecs (Proxy :: Proxy b) b
instance {-# OVERLAPPABLE #-} ColSpecs a ~ ColSpec a => TableSpec a where
  mergeSpecs _ (ColSpec a) = a

-- | A table with the given name and columns.
table :: forall a. TableSpec a => TableName -> ColSpecs a -> Table a
table name cs = Table
    { tableName = name
    , tableCols = tcs
    , tableHasAutoPK = Prelude.any ((AutoIncrement `elem`) . colAttrs) tcs
    }
  where
    tcs = validate name $ map tidy $ mergeSpecs (Proxy :: Proxy a) cs

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
  | otherwise = throw $ ValidationError $ concat
      [ "validation of table ", unpack $ fromTableName name, " failed:"
      , "\n  "
      , unpack $ intercalate "\n  " errs
      ]
  where
    colIdents = map (fromColName . colName) cis
    allIdents = fromTableName name : colIdents
    errs = concat
      [ dupes
      , pkDupes
      , optionalRequiredMutex
      , nulIdents
      , emptyIdents
      , emptyTableName
      , nonPkFks
      ]
    emptyTableName
      | fromTableName name == "\"\"" = ["table name is empty"]
      | otherwise                    = []
    emptyIdents
      | Prelude.any (== "\"\"") colIdents =
        ["table has columns with empty names"]
      | otherwise =
        []
    nulIdents =
      [ "table or column name contains \\NUL: " <> n
      | n <- allIdents
      , Data.Text.any (== '\NUL') n
      ]
    dupes =
      ["duplicate column: " <> fromColName x | (x:_:_) <- soup $ map colName cis]
    pkDupes =
      ["multiple primary keys" | (Primary:_:_) <- soup $ concatMap colAttrs cis]
    nonPkFks =
      [ "column is used as a foreign key, but is not primary or unique: "
          <> fromTableName ftn <> "." <> fromColName fcn
      | ci <- cis
      , (Table ftn fcs _, fcn) <- colFKs ci
      , fc <- fcs
      , colName fc == fcn
      , not (Unique `elem` colAttrs fc)
      ]

    -- This should be impossible, but...
    optionalRequiredMutex =
      [ "BUG: column " <> fromColName (colName ci)
                       <> " is both optional and required"
      | ci <- cis
      , Optional `elem` colAttrs ci && Required `elem` colAttrs ci
      ]
