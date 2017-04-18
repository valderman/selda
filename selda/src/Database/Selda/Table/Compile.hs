{-# LANGUAGE OverloadedStrings #-}
-- | Generating SQL for creating and deleting tables.
module Database.Selda.Table.Compile where
import Database.Selda.Table
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
    cols n = "(" <> Text.intercalate ", " (mkParams ncols n) <> ")"
    vals = Text.intercalate ", " $ zipWith (\f n -> f n)
                                           (replicate mrows cols)
                                           [0, ncols ..]
    mkParams cs n = map (pack . ('$':) . show . (+n)) [1..cs]

-- | Compile a column attribute.
compileColAttr :: ColAttr -> Text
compileColAttr Primary       = "PRIMARY KEY"
compileColAttr AutoIncrement = "AUTOINCREMENT"
compileColAttr Required      = "NOT NULL"
compileColAttr Optional      = "NULL"
