{-# LANGUAGE GADTs, FlexibleContexts, GeneralizedNewtypeDeriving, TypeOperators #-}
-- | Query monad and primitive operations.
module Database.Selda.Query where
import Database.Selda.Table
import Database.Selda.Column hiding (Result)
import Database.Selda.SQL
import Database.Selda.DeadCols
import Control.Monad.State

-- | An SQL query.
newtype Query a = Query {unQ :: State GenState a}
  deriving (Functor, Applicative, Monad)

-- | SQL generation internal state.
--   Contains the subqueries and static (i.e. not dependent on any subqueries)
--   restrictions of the query currently being built, as well as a name supply
--   for column renaming.
data GenState = GenState
  { sources         :: [SQL]
  , staticRestricts :: [Col Bool]
  , nameSupply      :: Int
  }

-- | Initial state: no subqueries, no restrictions.
initState :: GenState
initState = GenState
  { sources = []
  , staticRestricts = []
  , nameSupply = 0
  }

-- | Query the given table. Result is returned as an extensible tuple, i.e.
--   @first :*: second :*: third <- query tableOfThree@.
select :: Columns (Cols a) => Table a -> Query (Cols a)
select (Table name cs) = Query $ do
    rns <- mapM rename cs'
    st <- get
    put $ st {sources = SQL rns (Left name) [] : sources st}
    return $ toTup [n | Named n _ <- rns]
  where
    cs' = map snd cs

-- | Restrict the query somehow. Roughly equivalent to @WHERE@.
restrict :: Col Bool -> Query ()
restrict pred = Query $ do
    st <- get
    put $ case sources st of
      []            -> st {staticRestricts = pred : staticRestricts st}
      [SQL cs s ps] -> st {sources = [SQL cs s (pred : ps)]}
      ss            -> st {sources = [SQL (allCols ss) (Right ss) [pred]]}

-- | Generate a unique name for the given column.
--   Not for public consumption.
rename :: ColName -> State GenState SomeCol
rename col = do
  st <- get
  put $ st {nameSupply = succ $ nameSupply st}
  return $ Named (col ++ "_" ++ show (nameSupply st)) (Col col)

-- | Get all columns from a list of SQL ASTs.
allCols :: [SQL] -> [SomeCol]
allCols = concatMap cols

-- | Run a query computation from an initial state.
runQueryM :: Query a -> (a, GenState)
runQueryM = flip runState initState . unQ

-- | Compile a query into a parameterised SQL statement.
compile :: Result a => Query a -> (String, [Param])
compile = compSql . compQuery

-- | Compile a query to an SQL AST.
--   TODO: modify generated query instead of adding an outer one, but any
--         renamings must be reversed.
compQuery :: Result a => Query a -> SQL
compQuery q = SQL final (Right [removeDeadCols live sql]) []
  where
    (cs, st) = runQueryM q
    final = finalCols cs
    sql = state2sql st
    live = colNames $ final ++ map Some (restricts sql)

-- | Build the outermost query from the SQL generation state.
state2sql :: GenState -> SQL
state2sql (GenState [sql] srs _) = sql {restricts = restricts sql ++ srs}
state2sql (GenState ss srs _)    = SQL (allCols ss) (Right ss) srs

class Result a where
  finalCols :: a -> [SomeCol]

instance Result (Col a) where
  finalCols c = [Some c]

instance (Result a, Result b) => Result (a :*: b) where
  finalCols (a :*: b) = finalCols a ++ finalCols b
