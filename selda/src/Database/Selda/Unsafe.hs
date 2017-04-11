-- | Unsafe operations giving the user unchecked low-level control over
--   the generated SQL.
module Database.Selda.Unsafe
  ( fun, fun2
  , aggr
  , cast
  ) where
import Database.Selda.Column
import Database.Selda.Inner (aggr)

-- | Cast a column to another type, using whichever coercion semantics are used
--   by the underlying SQL implementation.
cast :: Col s a -> Col s b
cast = liftC Cast
