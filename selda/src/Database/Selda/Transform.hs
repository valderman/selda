-- | Analysis and transformation of SQL queries.
module Database.Selda.Transform where
import Database.Selda.Column
import Database.Selda.SQL
import Database.Selda.Query.Type
import Database.Selda.Types

-- | Remove all dead columns recursively, assuming that the given list of
--   column names contains all names present in the final result.
removeDeadCols :: [ColName] -> SQL -> SQL
removeDeadCols live sql =
    case source sql' of
      EmptyTable          -> sql'
      TableName _         -> sql'
      Values  _ _         -> sql'
      RawSql _            -> sql'
      Product qs          -> sql' {source = Product $ map noDead qs}
      Join jt on l r      -> sql' {source = Join jt on (noDead l) (noDead r)}
      Union union_all l r -> sql' {source = Union union_all (noDead l) (noDead r)}
  where
    noDead = removeDeadCols live'
    sql' = keepCols (implicitlyLiveCols sql ++ live) sql
    live' = allColNames sql'

-- | Return the names of all columns in the given top-level query.
--   Subqueries are not traversed.
allColNames :: SQL -> [ColName]
allColNames sql = colNames (cols sql) ++ implicitlyLiveCols sql

-- | Return the names of all non-output (i.e. 'cols') columns in the given
--   top-level query. Subqueries are not traversed.
implicitlyLiveCols :: SQL -> [ColName]
implicitlyLiveCols sql = concat
  [ concatMap allNamesIn (restricts sql)
  , colNames (groups sql)
  , colNames (map snd $ ordering sql)
  , colNames (liveExtras sql)
  , case source sql of
      Join _ on _ _ -> allNamesIn on
      _             -> []
  ]

-- | Get all column names appearing in the given list of (possibly complex)
--   columns.
colNames :: [SomeCol SQL] -> [ColName]
colNames cs = concat
  [ [n | Some c <- cs, n <- allNamesIn c]
  , [n | Named _ c <- cs, n <- allNamesIn c]
  , [n | Named n _ <- cs]
  ]

-- | Remove all columns but the given, named ones and aggregates, from a query's
--   list of outputs.
--   If we want to refer to a column in an outer query, it must have a name.
--   If it doesn't, then it's either not referred to by an outer query, or
--   the outer query duplicates the expression, thereby referring directly
--   to the names of its components.
keepCols :: [ColName] -> SQL -> SQL
keepCols live sql = sql {cols = filtered}
  where
    filtered = filter (`oneOf` live) (cols sql)
    oneOf (Some (AggrEx _ _)) _    = True
    oneOf (Named _ (AggrEx _ _)) _ = True
    oneOf (Some (Col n)) ns        = n `elem` ns
    oneOf (Named n _) ns           = n `elem` ns
    oneOf _ _                      = False

-- | Build the outermost query from the SQL generation state.
--   Groups are ignored, as they are only used by 'aggregate'.
state2sql :: GenState -> SQL
state2sql (GenState [sql] srs _ _ _) =
  sql {restricts = restricts sql ++ srs}
state2sql (GenState ss srs _ _ _) =
  SQL (allCols ss) (Product ss) srs [] [] Nothing [] False

-- | Get all output columns from a list of SQL ASTs.
allCols :: [SQL] -> [SomeCol SQL]
allCols sqls = [outCol col | sql <- sqls, col <- cols sql]
  where
    outCol (Named n _) = Some (Col n)
    outCol c           = c
