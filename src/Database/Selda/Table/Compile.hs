{-# LANGUAGE OverloadedStrings #-}
-- | Generating SQL for creating and deleting tables.
module Database.Selda.Table.Compile where
import Database.Selda.Table
import Database.Selda.Types
import Data.Text (Text, intercalate)
import qualified Data.Text as Text

data OnError = Fail | Ignore
  deriving (Eq, Ord, Show)

-- | Compile a @CREATAE TABLE@ query from a table definition.
compileCreateTable :: OnError -> Table a -> Text
compileCreateTable ifex tbl = mconcat
  [ "CREATE TABLE ", ifNotExists ifex, tableName tbl, "("
  , intercalate ", " (map compileTableCol (tableCols tbl))
  , ");"
  ]
  where
    ifNotExists Fail   = ""
    ifNotExists Ignore = "IF NOT EXISTS "

-- | Compile a table column.
compileTableCol :: ColInfo -> Text
compileTableCol ci = Text.unwords
  [ colName ci
  , colType ci
  , Text.unwords (map compileColAttr (colAttrs ci))
  ]

-- | Compile a @DROP TABLE@ query.
compileDropTable :: OnError -> Table a -> Text
compileDropTable Fail t = Text.unwords ["DROP TABLE",tableName t,";"]
compileDropTable _ t    = Text.unwords ["DROP TABLE IF EXISTS",tableName t,";"]

-- | Compile an @INSERT INTO@ query with @n@ parameters.
compInsert :: TableName -> Int -> Text
compInsert tbl args = Text.unwords ["INSERT INTO", tbl, "VALUES (", cs', ");"]
  where cs' = Text.intercalate ", " (replicate args "?")

-- | Compile a column attribute.
--   TODO: at least @AutoIncrement@ is implementation-specific; parameterise
--   over implementation quirks.
compileColAttr :: ColAttr -> Text
compileColAttr Primary       = "PRIMARY KEY"
compileColAttr AutoIncrement = "AUTOINCREMENT"
compileColAttr Required      = "NOT NULL"
compileColAttr Optional      = "NULL"
