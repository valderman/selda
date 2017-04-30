{-# LANGUAGE OverloadedStrings #-}
-- | Generating SQL for creating and deleting tables.
module Database.Selda.Table.Compile where
import Database.Selda.Table
import Data.List (foldl')
import Data.Monoid
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as Text
import Database.Selda.SQL hiding (params)
import Database.Selda.Types

data OnError = Fail | Ignore
  deriving (Eq, Ord, Show)

-- | Compile a @CREATE TABLE@ query from a table definition.
compileCreateTable :: (Text -> [ColAttr] -> Maybe Text) -> OnError -> Table a -> Text
compileCreateTable customColType ifex tbl = mconcat
  [ "CREATE TABLE ", ifNotExists ifex, fromTableName (tableName tbl), "("
  , intercalate ", " (map (compileTableCol customColType) (tableCols tbl))
  , ")"
  ]
  where
    ifNotExists Fail   = ""
    ifNotExists Ignore = "IF NOT EXISTS "

-- | Compile a table column.
compileTableCol :: (Text -> [ColAttr] -> Maybe Text) -> ColInfo -> Text
compileTableCol customColType ci = Text.unwords
  [ fromColName (colName ci)
  , case customColType typ attrs of
      Just s -> s
      _      -> typ <> " " <> Text.unwords (map compileColAttr attrs)
  ]
  where
    typ = colType ci
    attrs = colAttrs ci

-- | Compile a @DROP TABLE@ query.
compileDropTable :: OnError -> Table a -> Text
compileDropTable Fail t =
  Text.unwords ["DROP TABLE",fromTableName (tableName t)]
compileDropTable _ t =
  Text.unwords ["DROP TABLE IF EXISTS",fromTableName (tableName t)]

-- | Compile an @INSERT INTO@ query inserting @m@ rows with @n@ cols each.
--   Note that backends expect insertions to NOT have a semicolon at the end.
--   In addition to the compiled query, this function also returns the list of
--   parameters to be passed to the backend.
compInsert :: Text -> Table a -> [[Either Param Param]] -> (Text, [Param])
compInsert defaultKeyword tbl defs =
    (query, parameters)
  where
    colNames = map colName $ tableCols tbl
    values = Text.intercalate ", " vals
    (vals, parameters) = mkRows 1 defs [] []
    query = Text.unwords
      [ "INSERT INTO"
      , fromTableName (tableName tbl)
      , "(" <>  Text.intercalate ", " (map fromColName colNames) <> ")"
      , "VALUES"
      , values
      ]

    -- Build all rows: just recurse over the list of defaults (which encodes
    -- the # of elements in total as well), building each row, keeping track
    -- of the next parameter identifier.
    mkRows n (ps:pss) rts paramss =
      case mkRow n ps (tableCols tbl) of
        (n', names, params) -> mkRows n' pss (rowText:rts) (params:paramss)
          where rowText = "(" <> Text.intercalate ", " (reverse names) <> ")"
    mkRows _ _ rts ps =
      (reverse rts, reverse $ concat ps)

    -- Build a row: use the NULL/DEFAULT keyword for default rows, otherwise
    -- use a parameter.
    mkRow n ps names = foldl' mkCols (n, [], []) (zip ps names)

    -- Build a column: default values only available for for auto-incrementing
    -- primary keys.
    mkCol :: Int -> Either Param Param -> ColInfo -> [Param] -> (Int, Text, [Param])
    mkCol n (Left def) col ps
      | AutoIncrement `elem` colAttrs col =
        (n, defaultKeyword, ps)
      | otherwise =
        (n+1, pack ('$':show n), def:ps)
    mkCol n (Right val) _ ps =
        (n+1, pack ('$':show n), val:ps)

    -- Create a colum and return the next parameter id, plus the column itself.
    mkCols :: (Int, [Text], [Param]) -> (Either Param Param, ColInfo) -> (Int, [Text], [Param])
    mkCols (n, names, params) (param, col) =
      case mkCol n param col params of
        (n', name, params') -> (n', name:names, params')

-- | Compile a column attribute.
compileColAttr :: ColAttr -> Text
compileColAttr Primary       = "PRIMARY KEY"
compileColAttr AutoIncrement = "AUTOINCREMENT"
compileColAttr Required      = "NOT NULL"
compileColAttr Optional      = "NULL"
