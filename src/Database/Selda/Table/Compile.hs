{-# LANGUAGE OverloadedStrings #-}
-- | Generating SQL for creating and deleting tables.
module Database.Selda.Table.Compile where
import Database.Selda.Table
import Data.Monoid
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

-- | Compile an @INSERT INTO@ query inserting @m@ rows with @n@ cols each.
compInsert :: Table a -> Int -> Text
compInsert tbl mrows =
    Text.unwords ["INSERT INTO", tableName tbl, names, "VALUES", vals]
  where
    nonAutos =
      [ colName c
      | c <- tableCols tbl
      , not (AutoIncrement `elem` colAttrs c)
      ]
    ncols = length nonAutos
    names = "(" <>  Text.intercalate ", " nonAutos <> ")"
    cols = "(" <> Text.intercalate ", " (replicate ncols "?") <> ")"
    vals = Text.intercalate ", " (replicate mrows cols)

-- | Compile a column attribute.
--   TODO: at least @AutoIncrement@ is implementation-specific; parameterise
--   over implementation quirks.
compileColAttr :: ColAttr -> Text
compileColAttr Primary       = "PRIMARY KEY"
compileColAttr AutoIncrement = "AUTOINCREMENT"
compileColAttr Required      = "NOT NULL"
compileColAttr Optional      = "NULL"
