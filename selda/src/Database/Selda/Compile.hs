{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- | Selda SQL compilation.
module Database.Selda.Compile where
import Database.Selda.Column
import Database.Selda.Query.Type
import Database.Selda.SQL
import Database.Selda.SQL.Print
import Database.Selda.SqlType
import Database.Selda.Table
import Database.Selda.Table.Compile
import Database.Selda.Transform
import Database.Selda.Types
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Text (Text, empty)
import Data.Typeable

-- | Compile a query into a parameterised SQL statement.
compile :: Result a => Query s a -> (Text, [Param])
compile = snd . compileWithTables

-- | Compile a query into a parameterised SQL statement. Also returns all
--   tables depended on by the query.
compileWithTables :: Result a => Query s a -> ([TableName], (Text, [Param]))
compileWithTables = compSql . snd . compQuery

-- | Compile an @INSERT@ query, given the keyword representing default values
--   in the target SQL dialect, a table and a list of items corresponding
--   to the table.
compileInsert :: Insert a => Text -> Table a -> [a] -> (Text, [Param])
compileInsert _ _ []         = (empty, [])
compileInsert defkw tbl rows = compInsert defkw tbl (map params rows)

-- | Compile an @UPDATE@ query.
compileUpdate :: forall s a. (Columns (Cols s a), Result (Cols s a))
              => Table a                  -- ^ The table to update.
              -> (Cols s a -> Cols s a)   -- ^ Update function.
              -> (Cols s a -> Col s Bool) -- ^ Predicate: update only when true.
              -> (Text, [Param])
compileUpdate tbl upd check =
    compUpdate (tableName tbl) predicate updated
  where
    names = map colName (tableCols tbl)
    cs = toTup names
    updated = zip names (finalCols (upd cs))
    C predicate = check cs

-- | Compile a @DELETE FROM@ query.
compileDelete :: Columns (Cols s a)
              => Table a -> (Cols s a -> Col s Bool) -> (Text, [Param])
compileDelete tbl check = compDelete (tableName tbl) predicate
  where C predicate = check $ toTup $ map colName $ tableCols tbl

-- | Compile a query to an SQL AST.
--   Groups are ignored, as they are only used by 'aggregate'.
compQuery :: Result a => Query s a -> (Int, SQL)
compQuery q =
    (nameSupply st, SQL final (Product [srcs]) [] [] [] Nothing)
  where
    (cs, st) = runQueryM q
    final = finalCols cs
    sql = state2sql st
    live = colNames final ++ allNonOutputColNames sql
    srcs = removeDeadCols live sql

-- | An acceptable query result type; one or more columns stitched together
--   with @:*:@.
class Typeable (Res r) => Result r where
  type Res r
  -- | Converts the given list of @SqlValue@s into an tuple of well-typed
  --   results.
  --   See 'querySQLite' for example usage.
  --   The given list must contain exactly as many elements as dictated by
  --   the @Res r@. If the result is @a :*: b :*: c@, then the list must
  --   contain exactly three values, for instance.
  toRes :: Proxy r -> [SqlValue] -> Res r

  -- | Produce a list of all columns present in the result.
  finalCols :: r -> [SomeCol]

instance (Typeable a, SqlType a, Result b) => Result (Col s a :*: b) where
  type Res (Col s a :*: b) = a :*: Res b
  toRes _ (x:xs) = fromSql x :*: toRes (Proxy :: Proxy b) xs
  toRes _ _      = error "backend bug: too few result columns to toRes"
  finalCols (a :*: b) = finalCols a ++ finalCols b

instance (Typeable a, SqlType a) => Result (Col s a) where
  type Res (Col s a) = a
  toRes _ [x] = fromSql x
  toRes _ []  = error "backend bug: too few result columns to toRes"
  toRes _ _   = error "backend bug: too many result columns to toRes"
  finalCols (C c) = [Some c]
