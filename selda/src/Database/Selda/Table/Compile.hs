{-# LANGUAGE OverloadedStrings, CPP #-}
-- | Generating SQL for creating and deleting tables.
module Database.Selda.Table.Compile where
import Database.Selda.Table
import Database.Selda.Table.Validation
import Data.List (foldl')
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid
#endif
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as Text
import Database.Selda.SQL hiding (param)
import Database.Selda.SQL.Print.Config
import Database.Selda.SqlType (SqlTypeRep(..))
import Database.Selda.Types

data OnError = Fail | Ignore
  deriving (Eq, Ord, Show)

-- | Compile a sequence of queries to create the given table, including indexes.
--   The first query in the sequence is always @CREATE TABLE@.
compileCreateTable :: PPConfig -> OnError -> Table a -> Text
compileCreateTable cfg ifex tbl =
    ensureValid `seq` createTable
  where
    createTable = mconcat
      [ "CREATE TABLE ", ifNotExists ifex, fromTableName (tableName tbl), "("
      , intercalate ", " (map (compileTableCol cfg) (tableCols tbl) ++ multiUniques ++ multiPrimary)
      , case allFKs of
          [] -> ""
          _  -> ", " <> intercalate ", " compFKs
      , ")"
      ]
    multiPrimary =
      [ mconcat ["PRIMARY KEY(", intercalate ", " (colNames ixs), ")"]
      | (ixs, Primary) <- tableAttrs tbl
      ]
    multiUniques =
      [ mconcat ["UNIQUE(", intercalate ", " (colNames ixs), ")"]
      | (ixs, Unique) <- tableAttrs tbl
      ]
    colNames ixs = [fromColName (colName (tableCols tbl !! ix)) | ix <- ixs]
    ifNotExists Fail   = ""
    ifNotExists Ignore = "IF NOT EXISTS "
    allFKs = [(colName ci, fk) | ci <- tableCols tbl, fk <- colFKs ci]
    compFKs = zipWith (uncurry compileFK) allFKs [0..]
    ensureValid = validateOrThrow (tableName tbl) (tableCols tbl)

-- | Compile the @CREATE INDEX@ queries for all indexes on the given table.
compileCreateIndexes :: PPConfig -> OnError -> Table a -> [Text]
compileCreateIndexes cfg ifex tbl =
  [ compileCreateIndex cfg ifex (tableName tbl) col mmethod
  | (col, mmethod) <- indexedCols tbl
  ]

-- | Get the name to use for an index on the given column in the given table.
indexNameFor :: TableName -> ColName -> Text
indexNameFor t c =
  fromColName $ addColPrefix c ("ix" <> rawTableName t <> "_")

-- | Compile a @CREATE INDEX@ query for the given index.
compileCreateIndex :: PPConfig
                   -> OnError
                   -> TableName
                   -> ColName
                   -> Maybe IndexMethod
                   -> Text
compileCreateIndex cfg ifex tbl col mmethod = mconcat
  [ "CREATE INDEX ", indexNameFor tbl col, " ON ", fromTableName tbl
  , case mmethod of
      Just method -> " " <> ppIndexMethodHook cfg method
      _           -> ""
  , " (", fromColName col, ")"
  , if ifex == Ignore then " IF NOT EXISTS" else ""
  ]

-- | Compile a foreign key constraint.
compileFK :: ColName -> (Table (), ColName) -> Int -> Text
compileFK col (Table ftbl _ _ _, fcol) n = mconcat
  [ "CONSTRAINT ", fkName, " FOREIGN KEY (", fromColName col, ") "
  , "REFERENCES ", fromTableName ftbl, "(", fromColName fcol, ")"
  ]
  where
    fkName = fromColName $ addColPrefix col ("fk" <> pack (show n) <> "_")

-- | Compile a table column.
compileTableCol :: PPConfig -> ColInfo -> Text
compileTableCol cfg ci = Text.unwords
    [ fromColName (colName ci)
    , typeHook <> " " <> colAttrsHook
    ]
  where
    typeHook = ppTypeHook cfg cty attrs (ppType' cfg)
    colAttrsHook = ppColAttrsHook cfg cty attrs (ppColAttrs cfg)
    cty = colType ci
    attrs = colAttrs ci
    ppType'
      | cty == TRowID && AutoPrimary `elem` attrs = ppTypePK
      | otherwise = ppType

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
compInsert :: PPConfig -> Table a -> [[Either Param Param]] -> (Text, [Param])
compInsert cfg tbl defs =
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
      | AutoPrimary `elem` colAttrs col =
        (n, ppAutoIncInsert cfg, ps)
      | otherwise =
        (n+1, pack ('$':show n), def:ps)
    mkCol n (Right val) _ ps =
        (n+1, pack ('$':show n), val:ps)

    -- Create a colum and return the next parameter id, plus the column itself.
    mkCols :: (Int, [Text], [Param]) -> (Either Param Param, ColInfo) -> (Int, [Text], [Param])
    mkCols (n, names, params) (param, col) =
      case mkCol n param col params of
        (n', name, params') -> (n', name:names, params')
