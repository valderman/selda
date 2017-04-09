{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Database.Selda.Aggregates where
import Database.Selda.Column
import Database.Selda.Types
import Data.Text (Text)
import Unsafe.Coerce

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

-- | Convert one or more inner column to equivalent columns in the outer query.
--   @OuterCols (Aggr (Inner s) a :*: Aggr (Inner s) b) = Col s a :*: Col s b@,
--   for instance.
type family OuterCols a where
  OuterCols (t (Inner s) a :*: b) = Col s a :*: OuterCols b
  OuterCols (t (Inner s) a)       = Col s a

-- | The results of a join are always nullable, as there is no guarantee that
--   all joined columns will be non-null.
--   @JoinCols a@ where @a@ is an extensible tuple is that same tuple, but in
--   the outer query and with all elements nullable.
--   For instance:
--
-- >  JoinCols (Col (Inner s) Int :*: Col (Inner s) Text)
-- >    = Col s (Maybe Int) :*: Col s (Maybe Text)
type family JoinCols a where
  JoinCols (Col (Inner s) (Maybe a) :*: b) = Col s (Maybe a) :*: JoinCols b
  JoinCols (Col (Inner s) a :*: b)         = Col s (Maybe a) :*: JoinCols b
  JoinCols (Col (Inner s) (Maybe a))       = Col s (Maybe a)
  JoinCols (Col (Inner s) a)               = Col s (Maybe a)

-- | Coerce an inner column tuple to an outer one where all fields are nullable.
--   This is ONLY safe as long as 'Columns' only has instances for tuples of
--   actual columns.
toJoinCols :: (Columns a, Columns (JoinCols a)) => a -> JoinCols a
toJoinCols = unsafeCoerce

-- | Like 'toJoinCols', but does not make any fields nullable unless they
--   weren't before.
toOuterCols :: (Columns a, Columns (JoinCols a)) => a -> OuterCols a
toOuterCols = unsafeCoerce

-- | One or more aggregate columns.
class Aggregates a where
  unAggrs :: a -> [SomeCol]
instance Aggregates (Aggr (Inner s) a) where
  unAggrs (Aggr x) = [Some x]
instance Aggregates b => Aggregates (Aggr (Inner s) a :*: b) where
  unAggrs (Aggr a :*: b) = Some a : unAggrs b
