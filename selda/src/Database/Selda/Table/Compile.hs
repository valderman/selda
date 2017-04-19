{-# LANGUAGE OverloadedStrings #-}
-- | Generating SQL for creating and deleting tables.
module Database.Selda.Table.Compile where
import Database.Selda.Table
import Data.List (foldl')
import Data.Monoid
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as Text

data OnError = Fail | Ignore
  deriving (Eq, Ord, Show)

-- | Compile a @CREATE TABLE@ query from a table definition.
compileCreateTable :: (Text -> [ColAttr] -> Maybe Text) -> OnError -> Table a -> Text
compileCreateTable customColType ifex tbl = mconcat
  [ "CREATE TABLE ", ifNotExists ifex, tableName tbl, "("
  , intercalate ", " (map (compileTableCol customColType) (tableCols tbl))
  , ")"
  ]
  where
    ifNotExists Fail   = ""
    ifNotExists Ignore = "IF NOT EXISTS "

-- | Compile a table column.
compileTableCol :: (Text -> [ColAttr] -> Maybe Text) -> ColInfo -> Text
compileTableCol customColType ci = Text.unwords
  [ colName ci
  , case customColType typ attrs of
      Just s -> s
      _      -> typ <> " " <> Text.unwords (map compileColAttr attrs)
  ]
  where
    typ = colType ci
    attrs = colAttrs ci

-- | Compile a @DROP TABLE@ query.
compileDropTable :: OnError -> Table a -> Text
compileDropTable Fail t = Text.unwords ["DROP TABLE",tableName t]
compileDropTable _ t    = Text.unwords ["DROP TABLE IF EXISTS",tableName t]

-- | Compile an @INSERT INTO@ query inserting @m@ rows with @n@ cols each.
--   Note that backends expect insertions to NOT have a semicolon at the end.
compInsert :: Text -> Table a -> [[Bool]] -> Text
compInsert defaultKeyword tbl defs =
    Text.unwords ["INSERT INTO", tableName tbl, names, "VALUES", values]
  where
    colNames = map colName $ tableCols tbl
    names = "(" <>  Text.intercalate ", " colNames <> ")"
    values = Text.intercalate ", " (mkRows (1 :: Int) defs)

    -- Build all rows: just recurse over the list of defaults (which encodes
    -- the # of elements in total as well), building each row, keeping track
    -- of the next parameter identifier.
    mkRows n (ds:dss) =
      case mkRow n ds (tableCols tbl) of
        (n', vals) -> mkRowText (reverse vals) : mkRows n' dss
    mkRows _ _ =
      []

    mkRowText vals = "(" <> Text.intercalate ", " vals <> ")"

    -- Build a row: use the NULL/DEFAULT keyword for default rows, otherwise
    -- use a parameter.
    mkRow n ds cs = foldl' mkCols (n, []) (zip ds cs)

    -- Build a column: default values only available for for auto-incrementing
    -- primary keys.
    mkCol n def col
      | def && not (AutoIncrement `elem` colAttrs col) =
        error "only auto-incrementing primary keys may have defaults"
      | def =
        (n, defaultKeyword)
      | otherwise =
        (n+1, pack ('$':show n))

    -- Create a colum and return the next parameter id, plus the column itself.
    mkCols (n, cols) (def, col) =
      fmap (:cols) (mkCol n def col)

-- | Compile a column attribute.
compileColAttr :: ColAttr -> Text
compileColAttr Primary       = "PRIMARY KEY"
compileColAttr AutoIncrement = "AUTOINCREMENT"
compileColAttr Required      = "NOT NULL"
compileColAttr Optional      = "NULL"
