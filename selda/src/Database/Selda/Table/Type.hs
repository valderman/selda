module Database.Selda.Table.Type where
import Database.Selda.SqlType (SqlTypeRep)
import Database.Selda.Types

-- | A database table, based on some Haskell data type.
--   Any single constructor type can form the basis of a table, as long as
--   it derives @Generic@ and all of its fields are instances of @SqlType@.
data Table a = Table
  { -- | Name of the table. NOT guaranteed to be a valid SQL name.
    tableName :: TableName
    -- | All table columns.
    --   Invariant: the 'colAttrs' list of each column is sorted and contains
    --   no duplicates.
  , tableCols :: [ColInfo]
    -- | Does the given table have an auto-incrementing primary key?
  , tableHasAutoPK :: Bool
  }

-- | A complete description of a database column.
data ColInfo = ColInfo
  { colName  :: ColName
  , colType  :: SqlTypeRep
  , colAttrs :: [ColAttr]
  , colFKs   :: [(Table (), ColName)]
  }

-- | Column attributes such as nullability, auto increment, etc.
--   When adding elements, make sure that they are added in the order
--   required by SQL syntax, as this list is only sorted before being
--   pretty-printed.
data ColAttr
  = Primary
  | AutoIncrement
  | Required
  | Optional
  | Unique
  | Indexed (Maybe IndexMethod)
  deriving (Show, Eq, Ord)

-- | Method to use for indexing with 'indexedUsing'.
--   Index methods are ignored by the SQLite backend, as SQLite doesn't support
--   different index methods.
data IndexMethod
  = BTreeIndex
  | HashIndex
-- Omitted until the operator class business is sorted out
--  | GistIndex
--  | GinIndex
  deriving (Show, Eq, Ord)
