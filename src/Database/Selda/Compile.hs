{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
-- | Selda SQL compilation.
module Database.Selda.Compile where
import Database.Selda.Types
import Database.Selda.SqlType
import Database.Selda.Query.Type
import Database.Selda.SQL
import Database.Selda.SQL.Print
import Database.Selda.Column
import Database.Selda.Transform
import Data.Text (Text)
import Data.Proxy

-- | Compile a query into a parameterised SQL statement.
compile :: Result a => Query s a -> (Text, [Param])
compile = compSql . snd . compQuery 0

-- | Compile a query to an SQL AST.
--   Groups are ignored, as they are only used by 'aggregate'.
compQuery :: Result a => Int -> Query s a -> (Int, SQL)
compQuery ns q =
    (nameSupply st, SQL final (Right [srcs]) [] [] [] Nothing)
  where
    (cs, st) = runQueryM ns q
    final = finalCols cs
    sql = state2sql st
    live = colNames $ final ++ map Some (restricts sql)
    srcs = removeDeadCols live sql

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
