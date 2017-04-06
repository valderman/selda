{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | Query monad and primitive operations.
module Database.Selda.Query where
import Database.Selda.Table
import Database.Selda.Column
import Database.Selda.SQL
import Database.Selda.Aggregates
import Database.Selda.Query.Type
import Database.Selda.Transform
import Control.Monad.State
import Data.Text (Text, pack)
import Data.Monoid

-- | Query the given table. Result is returned as an inductive tuple, i.e.
--   @first :*: second :*: third <- query tableOfThree@.
select :: Columns (Cols s a) => Table a -> Query s (Cols s a)
select (Table name cs) = Query $ do
    rns <- mapM (rename . Some . Col) cs'
    st <- get
    put $ st {sources = SQL rns (Left name) [] [] [] Nothing : sources st}
    return $ toTup [n | Named n _ <- rns]
  where
    cs' = map snd cs

-- | Restrict the query somehow. Roughly equivalent to @WHERE@.
restrict :: Col s Bool -> Query s ()
restrict (C pred) = Query $ do
    st <- get
    put $ case sources st of
      [] ->
        st {staticRestricts = pred : staticRestricts st}
      [SQL cs s ps gs os lim] ->
        st {sources = [SQL cs s (pred : ps) gs os lim]}
      ss ->
        st {sources = [SQL (allCols ss) (Right ss) [pred] [] [] Nothing]}

-- | Execute a query, returning an aggregation of its results.
--   The query must return an inductive tuple of 'Aggregate' columns.
--   When @aggregate@ returns, those columns are converted into non-aggregate
--   columns, which may then be used to further restrict the query.
--
--   Note that aggregate queries must not depend on outer queries, nor must
--   they return any non-aggregate columns. Attempting to do either results in
--   a type error.
--
--   The SQL @HAVING@ keyword can be implemented by combining @aggregte@
--   and 'restrict':
--
-- > -- Find the number of people living on every address, for all addresses
-- < -- with more than one tenant:
-- > -- SELECT COUNT(name) AS c, address FROM housing GROUP BY name HAVING c > 1
-- >
-- > numPpl = do
-- >   num_tenants :*: address <- aggregate $ do
-- >     _ :*: address <- select housing
-- >     groupBy address
-- >     return (count address :*: some address)
-- >  restrict (num_tenants .> 1)
-- >  return (num_tenants :*: address)
aggregate :: (Columns (AggrCols a), Aggregates a)
          => Query (Inner s) a
          -> Query s (AggrCols a)
aggregate q = Query $ do
  -- Run query in isolation, then rename the remaining vars and generate outer
  -- query.
  st <- get
  let (aggrs, gst) = runQueryM (nameSupply st) q
  cs <- mapM rename $ unAggrs aggrs
  let ns' = nameSupply gst
      sql = state2sql gst
      sql' = SQL cs (Right [sql]) [] (groupCols gst) [] Nothing
  put $ st {sources = sql' : sources st, nameSupply = ns'}
  pure $ toTup [n | Named n _ <- cs]

-- | Group an aggregate query by a column.
--   Attempting to group a non-aggregate query is a type error.
groupBy :: Col (Inner s) a -> Query (Inner s) ()
groupBy (C c) = Query $ do
  st <- get
  put $ st {groupCols = Some c : groupCols st}

-- | Drop the first @m@ rows, then get at most @n@ of the remaining rows.
limit :: Int -> Int -> Query s ()
limit from to = Query $ do
  st <- get
  put $ case sources st of
    [SQL cs s ps gs os Nothing] ->
      st {sources = [SQL cs s ps gs os (Just (from, to))]}
    ss ->
      st {sources = [SQL (allCols ss) (Right ss) [] [] [] (Just (from, to))]}

-- | Sort the result rows in ascending or descending order on the given row.
order :: Col s a -> Order -> Query s ()
order (C c) o = Query $ do
  st <- get
  put $ case sources st of
    [SQL cs s ps gs os lim] ->
      st {sources = [SQL cs s ps gs ((o, Some c):os) lim]}
    ss ->
      st {sources = [SQL (allCols ss) (Right ss) [] [] [(o, Some c)] Nothing]}

-- | Generate a unique name for the given column.
--   Not for public consumption.
rename :: SomeCol -> State GenState SomeCol
rename (Some col) = do
  st <- get
  put $ st {nameSupply = succ $ nameSupply st}
  return $ Named (newName $ nameSupply st) col
  where
    newName ns =
      case col of
        Col n -> n <> "_" <> pack (show ns)
        _     -> "tmp_" <> pack (show ns)
rename col@(Named _ _) = do
  return col
