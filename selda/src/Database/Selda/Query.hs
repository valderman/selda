{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | Query monad and primitive operations.
module Database.Selda.Query
  (select, selectValues
  , restrict, groupBy, limit, order
  , aggregate, leftJoin, inner, suchThat
  ) where
import Database.Selda.Column
import Database.Selda.Inner
import Database.Selda.Query.Type
import Database.Selda.SQL
import Database.Selda.Table
import Database.Selda.Transform
import Control.Monad.State.Strict

-- | Query the given table. Result is returned as an inductive tuple, i.e.
--   @first :*: second :*: third <- query tableOfThree@.
select :: Columns (Cols s a) => Table a -> Query s (Cols s a)
select (Table name cs) = Query $ do
    rns <- mapM (rename . Some . Col) cs'
    st <- get
    put $ st {sources = SQL rns (TableName name) [] [] [] Nothing : sources st}
    return $ toTup [n | Named n _ <- rns]
  where
    cs' = map colName cs

-- | Query an ad hoc table of type @a@. Each element in the given list represents
--   one row in the ad hoc table.
selectValues :: (Insert a, Columns (Cols s a)) => [a] -> Query s (Cols s a)
selectValues [] = Query $ do
  st <- get
  put $ st {sources = SQL [] EmptyTable [] [] [] Nothing : sources st}
  return $ toTup (repeat "NULL")
selectValues (row:rows) = Query $ do
    names <- mapM (const freshName) firstrow
    let rns = [Named n (Col n) | n <- names]
        row' = mkFirstRow names
    s <- get
    put $ s {sources = SQL rns (Values row' rows') [] [] [] Nothing : sources s}
    return $ toTup [n | Named n _ <- rns]
  where
    firstrow = map defToVal $ params row
    mkFirstRow ns =
      [ Named n (Lit l)
      | (Param l, n) <- zip firstrow ns
      ]
    rows' = map (map defToVal . params) rows
    defToVal (Left x)  = x
    defToVal (Right x) = x

-- | Restrict the query somehow. Roughly equivalent to @WHERE@.
restrict :: Col s Bool -> Query s ()
restrict (C p) = Query $ do
    st <- get
    put $ case sources st of
      [] ->
        st {staticRestricts = p : staticRestricts st}
      -- PostgreSQL doesn't put renamed columns in scope in the WHERE clause
      -- of the query where they are renamed, so if the restrict predicate
      -- contains any vars renamed in this query, we must add another query
      -- just for the restrict.
      [SQL cs s ps gs os lim] | not $ p `wasRenamedIn` cs ->
        st {sources = [SQL cs s (p : ps) gs os lim]}
      ss ->
        st {sources = [SQL (allCols ss) (Product ss) [p] [] [] Nothing]}
  where
    wasRenamedIn predicate cs =
      let cs' = [n | Named n _ <- cs]
      in  any (`elem` cs') (colNames [Some predicate])

-- | Execute a query, returning an aggregation of its results.
--   The query must return an inductive tuple of 'Aggregate' columns.
--   When @aggregate@ returns, those columns are converted into non-aggregate
--   columns, which may then be used to further restrict the query.
--
--   Note that aggregate queries must not depend on outer queries, nor must
--   they return any non-aggregate columns. Attempting to do either results in
--   a type error.
--
--   The SQL @HAVING@ keyword can be implemented by combining @aggregate@
--   and 'restrict':
--
-- > -- Find the number of people living on every address, for all addresses
-- > -- with more than one tenant:
-- > -- SELECT COUNT(name) AS c, address FROM housing GROUP BY name HAVING c > 1
-- >
-- > numPpl = do
-- >   num_tenants :*: address <- aggregate $ do
-- >     _ :*: address <- select housing
-- >     groupBy address
-- >     return (count address :*: some address)
-- >  restrict (num_tenants .> 1)
-- >  return (num_tenants :*: address)
aggregate :: (Columns (OuterCols a), Aggregates a)
          => Query (Inner s) a
          -> Query s (OuterCols a)
aggregate q = Query $ do
  -- Run query in isolation, then rename the remaining vars and generate outer
  -- query.
  st <- get
  (gst, aggrs) <- isolate q
  cs <- mapM rename $ unAggrs aggrs
  let sql = state2sql gst
      sql' = SQL cs (Product [sql]) [] (groupCols gst) [] Nothing
  put $ st {sources = sql' : sources st}
  pure $ toTup [n | Named n _ <- cs]

-- | Perform a @LEFT JOIN@ with the current result set (i.e. the outer query)
--   as the left hand side, and the given query as the right hand side.
--   Like with 'aggregate', the inner (or right) query must not depend on the
--   outer (or right) one.
--
--   The given predicate over the values returned by the inner query determines
--   for each row whether to join or not. This predicate may depend on any
--   values from the outer query.
--
--   For instance, the following will list everyone in the @people@ table
--   together with their address if they have one; if they don't, the address
--   field will be @NULL@.
--
-- > getAddresses :: Query s (Col s Text :*: Col s (Maybe Text))
-- > getAddresses = do
-- >   name :*: _ <- select people
-- >   _ :*: address <- leftJoin (\(n :*: _) -> n .== name)
-- >                             (select addresses)
-- >   return (name :*: address)
leftJoin :: (Columns a, Columns (OuterCols a), Columns (LeftCols a))
         => (OuterCols a -> Col s Bool)
            -- ^ Predicate determining which lines to join.
            -- | Right-hand query to join.
         -> Query (Inner s) a
         -> Query s (LeftCols a)
leftJoin = someJoin LeftJoin

-- | Perform an @INNER JOIN@ with the current result set and the given query.
inner :: (Columns a, Columns (OuterCols a))
      => (OuterCols a -> Col s Bool)
            -- ^ Predicate determining which lines to join.
            -- | Right-hand query to join.
      -> Query (Inner s) a
      -> Query s (OuterCols a)
inner = someJoin InnerJoin

-- | Synonym for 'inner' for infix use:
--
-- > person <- select people
-- > home <- select homes `suchThat` \home -> home ! owner .== person ! name
-- > return (person ! name :*: home ! isApartment)
suchThat :: (Columns a, Columns (OuterCols a))
         => (OuterCols a -> Col s Bool)
         -> Query (Inner s) a
         -> Query s (OuterCols a)
suchThat = inner

-- | The actual code for any join.
someJoin :: (Columns a, Columns (OuterCols a), Columns a')
         => JoinType
         -> (OuterCols a -> Col s Bool)
         -> Query (Inner s) a
         -> Query s a'
someJoin jointype check q = Query $ do
  (join_st, res) <- isolate q
  cs <- mapM rename $ fromTup res
  st <- get
  let nameds = [n | Named n _ <- cs]
      left = state2sql st
      right = SQL cs (Product [state2sql join_st]) [] [] [] Nothing
      C on = check $ toTup nameds
      outCols = [Some $ Col n | Named n _ <- cs] ++ allCols [left]
      sql = SQL outCols (Join jointype on left right) [] [] [] Nothing
  put $ st {sources = [sql]}
  pure $ toTup nameds

-- | Group an aggregate query by a column.
--   Attempting to group a non-aggregate query is a type error.
--   An aggregate representing the grouped-by column is returned, which can be
--   returned from the aggregate query. For instance, if you want to find out
--   how many people have a pet at home:
--
-- > aggregate $ do
-- >   name :*: pet_name <- select people
-- >   name' <- groupBy name
-- >   return (name' :*: count(pet_name) > 0)
groupBy :: Col (Inner s) a -> Query (Inner s) (Aggr (Inner s) a)
groupBy (C c) = Query $ do
  st <- get
  put $ st {groupCols = Some c : groupCols st}
  return (Aggr c)

-- | Drop the first @m@ rows, then get at most @n@ of the remaining rows from the
--   given subquery.
limit :: Int -> Int -> Query (Inner s) a -> Query s a
limit from to q = Query $ do
  (lim_st, res) <- isolate q
  st <- get
  let sql = case sources lim_st of
        [SQL cs s ps gs os Nothing] ->
          SQL cs s ps gs os (Just (from, to))
        ss ->
          SQL (allCols ss) (Product ss) [] [] [] (Just (from, to))
  put $ st {sources = sql : sources st}
  return res

-- | Sort the result rows in ascending or descending order on the given row.
order :: Col s a -> Order -> Query s ()
order (C c) o = Query $ do
  st <- get
  put $ case sources st of
    [SQL cs s ps gs os lim] ->
      st {sources = [SQL cs s ps gs ((o, Some c):os) lim]}
    ss ->
      st {sources = [SQL (allCols ss) (Product ss) [] [] [(o, Some c)] Nothing]}
