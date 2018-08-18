{-# LANGUAGE OverloadedStrings, TupleSections #-}
-- | Utilities for validating and inspecting Selda tables.
module Database.Selda.Validation
  ( TableDiff (..), ColumnDiff (..)
  , TableName, ColName, ColumnInfo, SqlTypeRep, columnInfo
  , showTableDiff, showColumnDiff
  , describeTable, diffTable, diffTables
  , validateTable, validateSchema
  ) where
import Control.Monad.Catch
import Data.List ((\\))
import Data.Maybe (catMaybes)
import Data.Text (pack, unpack)
import Database.Selda
import Database.Selda.Backend
import Database.Selda.Table.Type (tableName, tableCols)
import Database.Selda.Table.Validation (ValidationError (..), validateOrThrow)

-- | Are the given types compatible?
isCompatibleWith :: SqlTypeRep -> SqlTypeRep -> Bool
isCompatibleWith TRowID TInt = True
isCompatibleWith TInt TRowID = True
isCompatibleWith a b         = a == b

-- | Validate a table schema, and check it for consistency against the current
--   database.
--   Throws a 'ValidationError' if the schema does not validate, or if
--   inconsistencies were found.
validateTable :: (MonadSelda m, MonadThrow m) => Table a -> m ()
validateTable t = do
  validateSchema t
  diffs <- diffTable t
  case diffs of
    TableOK -> return ()
    errors  -> throwM $ ValidationError $ concat
      [ "error validating table ", unpack (fromTableName (tableName t)), ":\n"
      , show errors
      ]

-- | Ensure that the schema of the given table is valid.
--   Does not ensure consistency with the current database.
validateSchema :: MonadThrow m => Table a -> m ()
validateSchema t = validateOrThrow (tableName t) (tableCols t) `seq` return ()

-- | A description of the difference between a schema and its corresponding
--   database table.
data TableDiff
  = TableOK
  | TableMissing
  | InconsistentColumns [(ColName, [ColumnDiff])]
    deriving Eq
instance Show TableDiff where
  show = unpack . showTableDiff

-- | A description of the difference between a column in a Selda table and its
--   corresponding database column.
data ColumnDiff
  = ColumnMissing
  | ColumnPresent
  | NameMismatch ColName
  | UnknownType Text
  | TypeMismatch SqlTypeRep SqlTypeRep
  | PrimaryKeyMismatch Bool
  | AutoIncrementMismatch Bool
  | NullableMismatch Bool
  | UniqueMismatch Bool
  | ForeignKeyMissing TableName ColName
  | ForeignKeyPresent TableName ColName
  | IndexMissing
  | IndexPresent
    deriving Eq

instance Show ColumnDiff where
  show = unpack . showColumnDiff

-- | Pretty-print a table diff.
showTableDiff :: TableDiff -> Text
showTableDiff TableOK = "no inconsistencies detected"
showTableDiff TableMissing = "table does not exist"
showTableDiff (InconsistentColumns cols) = mconcat
  [ "table has inconsistent columns:\n"
  , mconcat (map showColDiffs cols)
  ]
  where
    showColDiffs (col, diffs) = mconcat
      [ "  ", fromColName col, ":\n"
      , mconcat (map showDiffs diffs)
      ]
    showDiffs diff = mconcat
      [ "    ", showColumnDiff diff, "\n"
      ]

-- | Pretty-print a column diff.
showColumnDiff :: ColumnDiff -> Text
showColumnDiff ColumnMissing =
  "column does not exist in database"
showColumnDiff ColumnPresent =
  "column exists in database even though it shouldn't"
showColumnDiff IndexMissing =
  "column does not have an index in the database, even though it should"
showColumnDiff IndexPresent =
  "column has an index in the database, even though it shouldn't"
showColumnDiff (NameMismatch n) =
  mconcat ["column is called ", fromColName n, " in database"]
showColumnDiff (UnknownType t) =
  mconcat ["column has incompatible type \"", t, "\" in database"]
showColumnDiff (TypeMismatch t1 t2) =
  mconcat [ "column should have type `", pack (show t1)
          , "', but actually has type `", pack (show t2)
          , "' in database"
          ]
showColumnDiff (ForeignKeyMissing tbl col) =
  mconcat [ "column should be a foreign key referencing column "
          , fromColName col, " of table ", fromTableName tbl
          , "', but isn't a foreign key in database"
          ]
showColumnDiff (ForeignKeyPresent tbl col) =
  mconcat [ "column is a foreign key referencing column "
          , fromColName col, " of table ", fromTableName tbl
          , ", in database, even though it shouldn't be"
          ]
showColumnDiff (PrimaryKeyMismatch dbval) =
  showBoolDiff dbval "primary key"
showColumnDiff (AutoIncrementMismatch dbval) =
  showBoolDiff dbval "auto-incrementing"
showColumnDiff (NullableMismatch dbval) =
  showBoolDiff dbval "nullable"
showColumnDiff (UniqueMismatch dbval) =
  showBoolDiff dbval "unique"

showBoolDiff :: Bool -> Text -> Text
showBoolDiff True what =
  mconcat ["column is ", what, " in database, even though it shouldn't be"]
showBoolDiff False what =
  mconcat ["column is not ", what, " in database, even though it should be"]

-- | Get a description of the table by the given name currently in the database.
describeTable :: MonadSelda m => TableName -> m [ColumnInfo]
describeTable tbl = do
  b <- seldaBackend
  liftIO $ getTableInfo b tbl

-- | Check the given table for consistency with the current database, returning
--   a description of all inconsistencies found.
--   The table schema itself is not validated beforehand.
diffTable :: MonadSelda m => Table a -> m TableDiff
diffTable tbl = do
  dbInfos <- describeTable (tableName tbl)
  return $ diffColumns (columnInfo tbl) dbInfos

-- | Compute the difference between the two given tables.
--   The first table is considered to be the schema, and the second the database.
diffTables :: Table a -> Table b -> TableDiff
diffTables schema db = diffColumns (columnInfo schema) (columnInfo db)

-- | Compute the difference between the columns of two tables.
--   The first table is considered to be the schema, and the second the database.
diffColumns :: [ColumnInfo] -> [ColumnInfo] -> TableDiff
diffColumns infos dbInfos =
    case ( zipWith diffColumn infos dbInfos
         , map colName infos \\ map colName dbInfos
         , map colName dbInfos \\ map colName infos) of
      ([], _, _) ->
        TableMissing
      (diffs, [], []) | all consistent diffs ->
        TableOK
      (diffs, missing, extras) ->
        InconsistentColumns $ concat
          [ filter (not . consistent) diffs
          , map (, [ColumnMissing]) missing
          , map (, [ColumnPresent]) extras
          ]
  where
    consistent (_, diffs) = null diffs
    diffColumn schema db = (colName schema, catMaybes
      ([ check colName NameMismatch
       , case colType db of
           Left typ ->
             Just (UnknownType typ)
           Right t | not (t `isCompatibleWith` schemaColType) ->
             Just (TypeMismatch schemaColType t)
           _ ->
             Nothing
       , check colIsPK PrimaryKeyMismatch
       , check colIsAutoIncrement AutoIncrementMismatch
       , check colIsNullable NullableMismatch
       , check colIsUnique UniqueMismatch
       ] ++ mconcat
       [ map (Just . uncurry ForeignKeyPresent)
             (colFKs schema \\ colFKs db)
       , map (Just . uncurry ForeignKeyMissing)
             (colFKs db \\ colFKs schema)
       ]))
      where
        Right schemaColType = colType schema
        check :: Eq a
              => (ColumnInfo -> a)
              -> (a -> ColumnDiff)
              -> Maybe ColumnDiff
        check f err
          | f schema == f db = Nothing
          | otherwise        = Just (err (f db))
