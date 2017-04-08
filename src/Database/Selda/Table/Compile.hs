{-# LANGUAGE OverloadedStrings #-}
-- | Compiling queries for creating and deleting tables.
module Database.Selda.Table.Compile where
import Database.Selda.Table
import Data.Text (Text, intercalate)
import qualified Data.Text as Text

data IfExists = Fail | Ignore
  deriving (Eq, Ord, Show)

-- | Compie a @CREATAE TABLE@ query from a table definition.
compileCreateTable :: IfExists -> Table a -> Text
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
compileDropTable :: IfExists -> Table a -> Text
compileDropTable Fail t = Text.unwords ["DROP TABLE",tableName t,";"]
compileDropTable _ t    = Text.unwords ["DROP TABLE IF EXISTS",tableName t,";"]

-- | Compile a column attribute.
--   TODO: at least @AutoIncrement@ is implementation-specific; parameterise
--   over implementation quirks.
compileColAttr :: ColAttr -> Text
compileColAttr Primary       = "PRIMARY KEY"
compileColAttr AutoIncrement = "AUTOINCREMENT"
compileColAttr Required      = "NOT NULL"
compileColAttr Optional      = "NULL"
