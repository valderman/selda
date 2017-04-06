-- | Analysis and transformation of SQL queries.
module Database.Selda.Transform where
import Database.Selda.Column
import Database.Selda.SQL
import Database.Selda.Query.Type
import Database.Selda.Table

-- | Remove all dead columns recursively, assuming that the given list of
--   column names contains all names present in the final result.
removeDeadCols :: [ColName] -> SQL -> SQL
removeDeadCols cns sql =
    case source sql' of
      Left t   -> sql'
      Right qs -> sql' {source = Right $ map (removeDeadCols live') qs}
  where
    sql' = keepCols cns sql
    live' = allColNames sql'

-- | Return the names of all columns in the given top-level query.
--   Subqueries are not traversed.
allColNames :: SQL -> [ColName]
allColNames sql = concat
  [ colNames (map Some (restricts sql))
  , colNames (cols sql)
  , colNames (groups sql)
  ]

-- | Get all column names appearing in the given list of (possibly complex)
--   columns.
colNames :: [SomeCol] -> [ColName]
colNames cols = concat
  [ [n | Some c <- cols, n <- allNamesIn c]
  , [n | Named _ c <- cols, n <- allNamesIn c]
  , [n | Named n _ <- cols]
  ]

-- | Remove all columns but the given, named ones and aggregates.
--   If we want to refer to a column in an outer query, it must have a name.
--   If it doesn't, then it's either not referred to by an outer query, or
--   the outer query duplicates the expression, thereby referring directly
--   to the names of its components.
keepCols :: [ColName] -> SQL -> SQL
keepCols cns sql = sql {cols = filtered}
  where
    filtered =
      case filter (`oneOf` cns) (cols sql) of
        [] -> [Some (unC $ literal True)]
        cs -> cs
    oneOf (Some (AggrEx _ _)) _    = True
    oneOf (Named _ (AggrEx _ _)) _ = True
    oneOf (Some (Col n)) ns        = n `elem` ns
    oneOf (Named n _) ns           = n `elem` ns
    oneOf _ _                      = False

-- | Build the outermost query from the SQL generation state.
--   Groups are ignored, as they are only used by 'aggregate'.
state2sql :: GenState -> SQL
state2sql (GenState [sql] srs _ _) =
  sql {restricts = restricts sql ++ srs}
state2sql (GenState ss srs _ _) =
  SQL (allCols ss) (Right ss) srs [] [] Nothing

-- | Get all columns from a list of SQL ASTs.
allCols :: [SQL] -> [SomeCol]
allCols sqls = [outCol col | sql <- sqls, col <- cols sql]
  where
    outCol (Named n _) = Some (Col n)
    outCol c           = c
