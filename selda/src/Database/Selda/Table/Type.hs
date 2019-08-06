module Database.Selda.Table.Type where
import Database.Selda.SqlType (SqlTypeRep)
import Database.Selda.SQL (SQL)
import Database.Selda.Types
import Database.Selda.Exp

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

    -- | Attributes involving multiple columns.
  , tableAttrs :: [([Int], ColAttr)]
  }

-- | A complete description of a database column.
data ColInfo = ColInfo
  { colName  :: ColName
  , colType  :: SqlTypeRep
  , colAttrs :: [ColAttr]
  , colFKs   :: [(Table (), ColName)]
  , colExpr  :: UntypedCol SQL
  }

-- | Strongly or weakly auto-incrementing primary key?
data AutoIncType = Weak | Strong
  deriving (Show, Eq, Ord)

-- | Column attributes such as nullability, auto increment, etc.
--   When adding elements, make sure that they are added in the order
--   required by SQL syntax, as this list is only sorted before being
--   pretty-printed.
data ColAttr
  = Primary
  | AutoPrimary AutoIncType
  | Required
  | Optional
  | Unique
  | Indexed (Maybe IndexMethod)
  deriving (Show, Eq, Ord)

isAutoPrimary :: ColAttr -> Bool
isAutoPrimary (AutoPrimary _) = True
isAutoPrimary _               = False

isPrimary :: ColAttr -> Bool
isPrimary Primary = True
isPrimary attr    = isAutoPrimary attr

isUnique :: ColAttr -> Bool
isUnique Unique      = True
isUnique (Indexed _) = True
isUnique attr        = isPrimary attr

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
