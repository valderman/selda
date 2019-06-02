{-# LANGUAGE OverloadedStrings, CPP #-}
module Database.Selda.Table.Validation where
import Control.Exception
import Data.List (group, sort)
import Data.Text (Text, any, intercalate, unpack)
import Data.Typeable
import Database.Selda.Table.Type
import Database.Selda.Types
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid
#endif

-- | An error occurred when validating a database table.
--   If this error is thrown, there is a bug in your database schema, and the
--   particular table that triggered the error is unusable.
--   Since validation is deterministic, this error will be thrown on every
--   consecutive operation over the offending table.
--
--   Therefore, it is not meaningful to handle this exception in any way,
--   just fix your bug instead.
data ValidationError = ValidationError String
  deriving (Show, Eq, Typeable)
instance Exception ValidationError

-- | Ensure that there are no duplicate column names or primary keys.
--   Returns a list of validation errors encountered.
validate :: TableName -> [ColInfo] -> [Text]
validate name cis = errs
  where
    colIdents = map (fromColName . colName) cis
    allIdents = fromTableName name : colIdents
    errs = concat
      [ dupes
      , pkDupes
      , optionalRequiredMutex
      , nulIdents
      , emptyIdents
      , emptyTableName
      , nonPkFks
      ]
    emptyTableName
      | fromTableName name == "\"\"" = ["table name is empty"]
      | otherwise                    = []
    emptyIdents
      | Prelude.any (== "\"\"") colIdents =
        ["table has columns with empty names"]
      | otherwise =
        []
    nulIdents =
      [ "table or column name contains \\NUL: " <> n
      | n <- allIdents
      , Data.Text.any (== '\NUL') n
      ]
    dupes =
      ["duplicate column: " <> fromColName x | (x:_:_) <- soup $ map colName cis]
    pkDupes =
      if moreThanOne pkAttrs then ["multiple primary keys"] else []
    nonPkFks =
      [ "column is used as a foreign key, but is not primary or unique: "
          <> fromTableName ftn <> "." <> fromColName fcn
      | ci <- cis
      , (Table ftn fcs _ _, fcn) <- colFKs ci
      , fc <- fcs
      , colName fc == fcn
      , not $ Prelude.any isUnique (colAttrs fc)
      ]

    -- This should be impossible, but...
    optionalRequiredMutex =
      [ "BUG: column " <> fromColName (colName ci)
                       <> " is both optional and required"
      | ci <- cis
      , Optional `elem` colAttrs ci && Required `elem` colAttrs ci
      ]

    moreThanOne []  = False
    moreThanOne [_] = False
    moreThanOne _   = True
    pkAttrs =
      [ attr
      | attr <- concatMap colAttrs cis
      , isPrimary attr
      ]

-- | Return all columns of the given table if the table schema is valid,
--   otherwise throw a 'ValidationError'.
validateOrThrow :: TableName -> [ColInfo] -> [ColInfo]
validateOrThrow name cols =
  case validate name cols of
    []     -> cols
    errors -> throw $ ValidationError $ concat
      [ "validation of table `", unpack $ fromTableName name
      , "' failed:\n  "
      , unpack $ intercalate "\n  " errors
      ]

-- | Sort a list and remove all duplicates from it.
snub :: (Ord a, Eq a) => [a] -> [a]
snub = map head . soup

-- | Sort a list, then group all identical elements.
soup :: Ord a => [a] -> [[a]]
soup = group . sort
