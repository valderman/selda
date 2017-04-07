{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
-- | Selda SQL compilation.
module Database.Selda.Compile where
import Database.Selda.Table
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

-- | Some value that is representable in SQL.
data SqlValue where
  SqlInt    :: Int    -> SqlValue
  SqlFloat  :: Double -> SqlValue
  SqlString :: Text   -> SqlValue
  SqlBool   :: Bool   -> SqlValue
  SqlNull   :: SqlValue

-- | Any SQL result data type.
class SqlData a where
  fromSql :: SqlValue -> a
instance SqlData Int where
  fromSql (SqlInt x) = x
  fromSql _          = error "fromSql: int column with non-int value"
instance SqlData Double where
  fromSql (SqlFloat x) = x
  fromSql _            = error "fromSql: float column with non-float value"
instance SqlData Text where
  fromSql (SqlString x) = x
  fromSql _             = error "fromSql: text column with non-text value"
instance SqlData Bool where
  fromSql (SqlBool x) = x
  fromSql _           = error "fromSql: bool column with non-bool value"
instance SqlData a => SqlData (Maybe a) where
  fromSql SqlNull = Nothing
  fromSql x       = Just (fromSql x)

instance (SqlData a, Result b) => Result (Col s a :*: b) where
  type Res (Col s a :*: b) = a :*: Res b
  toRes _ (x:xs) = fromSql x :*: toRes (Proxy :: Proxy b) xs
  toRes _ _      = error "backend bug: too few result columns to toRes"
  finalCols (a :*: b) = finalCols a ++ finalCols b

instance SqlData a => Result (Col s a) where
  type Res (Col s a) = a
  toRes _ [x] = fromSql x
  toRes _ []  = error "backend bug: too few result columns to toRes"
  toRes _ _   = error "backend bug: too many result columns to toRes"
  finalCols (C c) = [Some c]
