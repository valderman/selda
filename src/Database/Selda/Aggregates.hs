{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances #-}
module Database.Selda.Aggregates where
import Database.Selda.Column
import Database.Selda.Table
import Data.Text (Text)

-- | A single aggregate column.
--   Aggregate columns may not be used to restrict queries.
--   When returned from an 'aggregate' subquery, an aggregate column is
--   converted into a non-aggregate column.
newtype Aggr s a = Aggr {unAggr :: Exp a}

-- | Denotes an inner query.
--   For aggregation, treating sequencing as the cartesian product of queries
--   does not work well.
--   Instead, we treat the sequencing of 'aggregate' with other
--   queries as the cartesian product of the aggregated result of the query,
--   a small but important difference.
--
--   However, for this to work, the aggregate query must not depend on any
--   columns in the outer product. Therefore, we let the aggregate query be
--   parameterized over @Inner s@ if the parent query is parameterized over @s@,
--   to enforce this separation.
data Inner s

-- | Create a named aggregate function.
--   Like 'fun', this function is generally unsafe and should ONLY be used
--   to implement missing backend-specific functionality.
aggr :: Text -> Col s a -> Aggr s b
aggr f = Aggr . AggrEx f . unC

-- | Convert one or more aggregate column to equivalent non-aggregate columns
--   in the outer query.
--   @AggrCols Aggr (Inner s) a :*: Aggr (Inner s) b = Col s a :*: Col s b@,
--   for instance.
type family AggrCols a where
  AggrCols (Aggr (Inner s) a :*: b) = Col s a :*: AggrCols b
  AggrCols (Aggr (Inner s) a)       = Col s a

-- | One or more aggregate columns.
class Aggregates a where
  unAggrs :: a -> [SomeCol]
instance Aggregates (Aggr (Inner s) a) where
  unAggrs (Aggr x) = [Some x]
instance Aggregates b => Aggregates (Aggr (Inner s) a :*: b) where
  unAggrs (Aggr a :*: b) = Some a : unAggrs b
