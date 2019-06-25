{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
-- | Query monad and primitive operations.
module Database.Selda.Query
  ( select, selectValues, Database.Selda.Query.distinct
  , restrict, groupBy, limit, order, orderRandom
  , aggregate, leftJoin, innerJoin
  ) where
import Data.Maybe (isNothing)
import Database.Selda.Column
import Database.Selda.Generic
import Database.Selda.Inner
import Database.Selda.Query.Type
import Database.Selda.SQL as SQL
import Database.Selda.SqlType (SqlType)
import Database.Selda.Table
import Database.Selda.Transform
import Control.Monad.State.Strict
import Data.Proxy
import GHC.Generics (Rep)
import Unsafe.Coerce

-- | Query the given table.
select :: Relational a => Table a -> Query s (Row s a)
select (Table name cs _ _) = Query $ do
  rns <- renameAll $ map colExpr cs
  st <- get
  put $ st {sources = sqlFrom rns (TableName name) : sources st}
  return $ Many (map hideRenaming rns)

-- | Query an ad hoc table of type @a@. Each element in the given list represents
--   one row in the ad hoc table.
selectValues :: forall s a. Relational a => [a] -> Query s (Row s a)
selectValues [] = Query $ do
  st <- get
  put $ st {sources = sqlFrom [] EmptyTable : sources st}
  return $ Many (gNew (Proxy :: Proxy (Rep a)))
selectValues (row:rows) = Query $ do
    names <- mapM (const freshName) firstrow
    let rns = [Named n (Col n) | n <- names]
        row' = mkFirstRow names
    s <- get
    put $ s {sources = sqlFrom rns (Values row' rows') : sources s}
    return $ Many (map hideRenaming rns)
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
restrict :: Same s t => Col s Bool -> Query t ()
restrict (One p) = Query $ do
    st <- get
    put $ case sources st of
      [] ->
        st {staticRestricts = p : staticRestricts st}
      -- PostgreSQL doesn't put renamed columns in scope in the WHERE clause
      -- of the query where they are renamed, so if the restrict predicate
      -- contains any vars renamed in this query, we must add another query
      -- just for the restrict.
      [sql] | not $ p `wasRenamedIn` cols sql ->
        st {sources = [sql {restricts = p : restricts sql}]}
      ss ->
        st {sources = [(sqlFrom (allCols ss) (Product ss)) {restricts = [p]}]}
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
-- >   (num_tenants :*: theAddress) <- aggregate $ do
-- >     h <- select housing
-- >     theAddress <- groupBy (h ! address)
-- >     return (count (h ! address) :*: theAddress)
-- >  restrict (num_tenants .> 1)
-- >  return (num_tenants :*: theAddress)
aggregate :: (Columns (AggrCols a), Aggregates a)
          => Query (Inner s) a
          -> Query s (AggrCols a)
aggregate q = Query $ do
  (gst, aggrs) <- isolate q
  cs <- renameAll $ unAggrs aggrs
  let sql = (sqlFrom cs (Product [state2sql gst])) {groups = groupCols gst}
  modify $ \st -> st {sources = sql : sources st}
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
-- >   (name :*: _) <- select people
-- >   (_ :*: address) <- leftJoin (\(n :*: _) -> n .== name)
-- >                               (select addresses)
-- >   return (name :*: address)
leftJoin :: (Columns a, Columns (OuterCols a), Columns (LeftCols a))
         => (OuterCols a -> Col s Bool)
            -- ^ Predicate determining which lines to join.
            -- | Right-hand query to join.
         -> Query (Inner s) a
         -> Query s (LeftCols a)
leftJoin = someJoin LeftJoin

-- | Perform an @INNER JOIN@ with the current result set and the given query.
innerJoin :: (Columns a, Columns (OuterCols a))
          => (OuterCols a -> Col s Bool)
             -- ^ Predicate determining which lines to join.
             -- | Right-hand query to join.
          -> Query (Inner s) a
          -> Query s (OuterCols a)
innerJoin = someJoin InnerJoin

-- | The actual code for any join.
someJoin :: (Columns a, Columns (OuterCols a), Columns a')
         => JoinType
         -> (OuterCols a -> Col s Bool)
         -> Query (Inner s) a
         -> Query s a'
someJoin jointype check q = Query $ do
  (join_st, res) <- isolate q
  cs <- renameAll $ fromTup res
  st <- get
  let nameds = [n | Named n _ <- cs]
      left = state2sql st
      right = sqlFrom cs (Product [state2sql join_st])
      One on = check $ toTup nameds
      outCols = [Some $ Col n | Named n _ <- cs] ++ allCols [left]
  put $ st {sources = [sqlFrom outCols (Join jointype on left right)]}
  pure $ toTup nameds

-- | Group an aggregate query by a column.
--   Attempting to group a non-aggregate query is a type error.
--   An aggregate representing the grouped-by column is returned, which can be
--   returned from the aggregate query. For instance, if you want to find out
--   how many people have a pet at home:
--
-- > aggregate $ do
-- >   person <- select people
-- >   name' <- groupBy (person ! name)
-- >   return (name' :*: count(person ! pet_name) .> 0)
groupBy :: (Same s t, SqlType a) => Col (Inner s) a -> Query (Inner t) (Aggr (Inner t) a)
groupBy (One c) = Query $ do
  st <- get
  put $ st {groupCols = Some c : groupCols st}
  return (Aggr c)

-- | Drop the first @m@ rows, then get at most @n@ of the remaining rows from the
--   given subquery.
limit :: Same s t => Int -> Int -> Query (Inner s) a -> Query t (OuterCols a)
limit from to q = Query $ do
  (lim_st, res) <- isolate q
  st <- get
  let sql' = case sources lim_st of
        [sql] | isNothing (limits sql) -> sql
        ss                             -> sqlFrom (allCols ss) (Product ss)
  put $ st {sources = sql' {limits = Just (from, to)} : sources st}
  -- TODO: replace with safe coercion
  return $ unsafeCoerce res

-- | Sort the result rows in ascending or descending order on the given row.
--
--   If multiple @order@ directives are given, later directives are given
--   precedence but do not cancel out earlier ordering directives.
--   To get a list of persons sorted primarily on age and secondarily on name:
--
-- > peopleInAgeAndNameOrder = do
-- >   person <- select people
-- >   order (person ! name) ascending
-- >   order (person ! age) ascending
-- >   return (person ! name)
--
--   For a table @[("Alice", 20), ("Bob", 20), ("Eve", 18)]@, this query
--   will always return @["Eve", "Alice", "Bob"]@.
--
--   The reason for later orderings taking precedence and not the other way
--   around is composability: @order@ should always sort the current
--   result set to avoid weird surprises when a previous @order@ directive
--   is buried somewhere deep in an earlier query.
--   However, the ordering must always be stable, to ensure that previous
--   calls to order are not simply erased.
order :: (Same s t, SqlType a) => Col s a -> Order -> Query t ()
order (One c) o = Query $ do
  st <- get
  case sources st of
    [sql] -> put st {sources = [sql {ordering = (o, Some c) : ordering sql}]}
    ss    -> put st {sources = [sql {ordering = [(o, Some c)]}]}
      where sql = sqlFrom (allCols ss) (Product ss)

-- | Sort the result rows in random order.
orderRandom :: Query s ()
orderRandom = order (One (NulOp (Fun0 "RANDOM") :: Exp SQL Int)) Asc

-- | Remove all duplicates from the result set.
distinct :: (Columns a, Columns (OuterCols a))
         => Query (Inner s) a
         -> Query s (OuterCols a)
distinct q = Query $ do
  (inner_st, res) <- isolate q
  st <- get
  let ss = sources inner_st
  put st {sources = [(sqlFrom (allCols ss) (Product ss)) {SQL.distinct = True}]}
  return (unsafeCoerce res)
