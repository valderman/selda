-- | Dead column elimination for Selda.
module Database.Selda.DeadCols (colNames, removeDeadCols) where
import Database.Selda.Column
import Database.Selda.SQL
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
allColNames sql = colNames (map Some (restricts sql)) ++ colNames (cols sql)

-- | Get all column names appearing in the given list of (possibly complex)
--   columns.
colNames :: [SomeCol] -> [ColName]
colNames cols = concat
  [ [n | Some c <- cols, n <- allNamesIn c]
  , [n | Named _ c <- cols, n <- allNamesIn c]
  , [n | Named n _ <- cols]
  ]

-- | Remove all columns but the given, named ones.
--   If we want to refer to a column in an outer query, it must have a name.
--   If it doesn't, then it's either not referred to by an outer query, or
--   the outer query duplicates the expression, thereby referring directly
--   to the names of its components.
keepCols :: [ColName] -> SQL -> SQL
keepCols cns sql = sql {cols = filtered}
  where
    filtered =
      case filter (`oneOf` cns) (cols sql) of
        [] -> [Some (literal True)]
        cs -> cs
    oneOf (Some (Col n)) ns = n `elem` ns
    oneOf (Named n _) ns    = n `elem` ns
    oneOf _ _               = False
