{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Selda SQL compilation.
module Database.Selda.Compile
  ( Result, Res
  , buildResult, compQuery, compQueryWithFreshScope
  , compile, compileWith
  , compileInsert, compileUpdate, compileDelete
  )
  where
import Control.Monad (liftM2)
import Database.Selda.Column
import Database.Selda.Generic
import Database.Selda.Query.Type
import Database.Selda.SQL
import Database.Selda.SQL.Print
import Database.Selda.SQL.Print.Config
import Database.Selda.SqlRow
import Database.Selda.SqlType
import Database.Selda.Table
import Database.Selda.Table.Compile
import Database.Selda.Transform
import Database.Selda.Types
import Data.Proxy
import Data.Text (Text, empty)
import Data.Typeable (Typeable)

-- For scope supply
import Data.IORef
import System.IO.Unsafe

-- | Compile a query into a parameterised SQL statement.
--
--   The types given are tailored for SQLite. To translate SQLite types into
--   whichever types are used by your backend, use 'compileWith'.
compile :: Result a => Query s a -> (Text, [Param])
compile = compileWith defPPConfig

-- | Compile a query using the given type translation function.
compileWith :: Result a => PPConfig -> Query s a -> (Text, [Param])
compileWith cfg = compSql cfg . snd . compQuery 0

-- | Compile an @INSERT@ query, given the keyword representing default values
--   in the target SQL dialect, a table and a list of items corresponding
--   to the table.
compileInsert :: Relational a => PPConfig -> Table a -> [a] -> [(Text, [Param])]
compileInsert _ _ [] =
  [(empty, [])]
compileInsert cfg tbl rows =
    case ppMaxInsertParams cfg of
      Nothing -> [compInsert cfg tbl rows']
      Just n  -> map (compInsert cfg tbl) (chunk (n `div` rowlen) rows')
  where
    rows' = map params rows
    rowlen = length (head rows')
    chunk chunksize xs =
      case splitAt chunksize xs of
        ([], []) -> []
        (x, [])  -> [x]
        (x, xs') -> x : chunk chunksize xs'

-- | Compile an @UPDATE@ query.
compileUpdate :: forall s a. (Relational a, SqlRow a)
              => PPConfig
              -> Table a                 -- ^ Table to update.
              -> (Row s a -> Row s a)    -- ^ Update function.
              -> (Row s a -> Col s Bool) -- ^ Predicate.
              -> (Text, [Param])
compileUpdate cfg tbl upd check =
    compUpdate cfg (tableName tbl) predicate updated
  where
    names = map colName (tableCols tbl)
    cs = tableExpr tbl
    updated = zip names (finalCols (upd cs))
    One predicate = check cs

-- | Compile a @DELETE FROM@ query.
compileDelete :: Relational a
              => PPConfig
              -> Table a
              -> (Row s a -> Col s Bool)
              -> (Text, [Param])
compileDelete cfg tbl check = compDelete cfg (tableName tbl) predicate
  where One predicate = check $ toTup $ map colName $ tableCols tbl

-- | Compile a query to an SQL AST.
--   Groups are ignored, as they are only used by 'aggregate'.
compQuery :: Result a => Scope -> Query s a -> (Int, SQL)
compQuery ns q =
    (nameSupply st, SQL final (Product [srcs]) [] [] [] Nothing False)
  where
    (cs, st) = runQueryM ns q
    final = finalCols cs
    sql = state2sql st
    live = colNames final ++ allNonOutputColNames sql
    srcs = removeDeadCols live sql

{-# NOINLINE scopeSupply #-}
scopeSupply :: IORef Scope
scopeSupply = unsafePerformIO $ newIORef 1

-- | Get a fresh scope from the global scope supply, then use it to compile
--   the given query.
compQueryWithFreshScope :: Result a => Query s a -> (Int, SQL)
compQueryWithFreshScope q = unsafePerformIO $ do
  s <- atomicModifyIORef' scopeSupply (\s -> (s+1, s))
  return $ compQuery s q

buildResult :: Result r => Proxy r -> [SqlValue] -> Res r
buildResult p = runResultReader (toRes p)

type family Res r where
  Res (Col s a :*: b) = a :*: Res b
  Res (Row s a :*: b) = a :*: Res b
  Res (Col s a)       = a
  Res (Row s a)       = a

-- | An acceptable query result type; one or more columns stitched together
--   with @:*:@.
class Typeable (Res r) => Result r where
  -- | Converts the given list of @SqlValue@s into an tuple of well-typed
  --   results.
  --   See 'querySQLite' for example usage.
  toRes :: Proxy r -> ResultReader (Res r)

  -- | Produce a list of all columns present in the result.
  finalCols :: r -> [SomeCol SQL]

instance (SqlType a, Result b) => Result (Col s a :*: b) where
  toRes _ = liftM2 (:*:) (fromSql <$> next) (toRes (Proxy :: Proxy b))
  finalCols (a :*: b) = finalCols a ++ finalCols b

instance (SqlRow a, Result b) => Result (Row s a :*: b) where
  toRes _ = liftM2 (:*:) nextResult (toRes (Proxy :: Proxy b))
  finalCols (a :*: b) = finalCols a ++ finalCols b

instance SqlType a => Result (Col s a) where
  toRes _ = fromSql <$> next
  finalCols (One c) = [Some c]

instance SqlRow a => Result (Row s a) where
  toRes _ = nextResult
  finalCols (Many cs) = [Some c | Untyped c <- cs]
