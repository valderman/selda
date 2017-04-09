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
import Data.Text (Text, empty)
import Data.Proxy

-- | Compile a query into a parameterised SQL statement.
compile :: Result a => Query s a -> (Text, [Param])
compile = compSql . snd . compQuery 0

-- | Compile an @INSERT@ query.
compileInsert :: Insert a => Table a -> [a] -> (Text, [Param])
compileInsert _ []     = (empty, [])
compileInsert tbl rows = (compInsert (tableName tbl) nrows ncols, concat ps)
  where ps = map params rows
        nrows = length rows
        ncols = length (head ps)

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
compQuery :: Result a => Int -> Query s a -> (Int, SQL)
compQuery ns q =
    (nameSupply st, SQL final (Product [srcs]) [] [] [] Nothing)
  where
    (cs, st) = runQueryM ns q
    final = finalCols cs
    sql = state2sql st
    live = colNames $ final ++ map Some (restricts sql)
    srcs = removeDeadCols live sql

-- | An extensible tuple of Haskell-level values (i.e. @Int :*: Maybe Text@)
--   which can be inserted into a table.
class Insert a where
  params :: a -> [Param]
instance (SqlType a, Insert b) => Insert (a :*: b) where
  params (a :*: b) = Param (mkLit a) : params b
instance {-# OVERLAPPABLE #-} SqlType a => Insert a where
  params a = [Param (mkLit a) ]

-- | An acceptable query result type; one or more columns stitched together
--   with @:*:@.
class Result r where
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

instance (SqlType a, Result b) => Result (Col s a :*: b) where
  type Res (Col s a :*: b) = a :*: Res b
  toRes _ (x:xs) = fromSql x :*: toRes (Proxy :: Proxy b) xs
  toRes _ _      = error "backend bug: too few result columns to toRes"
  finalCols (a :*: b) = finalCols a ++ finalCols b

instance SqlType a => Result (Col s a) where
  type Res (Col s a) = a
  toRes _ [x] = fromSql x
  toRes _ []  = error "backend bug: too few result columns to toRes"
  toRes _ _   = error "backend bug: too many result columns to toRes"
  finalCols (C c) = [Some c]
