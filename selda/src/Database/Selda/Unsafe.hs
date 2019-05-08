{-# LANGUAGE ScopedTypeVariables #-}
-- | Unsafe operations giving the user unchecked low-level control over
--   the generated SQL.
module Database.Selda.Unsafe
  ( fun, fun2, fun0, operator
  , aggr
  , cast, castAggr, sink, sink2
  , unsafeSelector
  ) where
import Database.Selda.Column
import Database.Selda.Inner (Inner, Aggr, aggr, liftAggr)
import Database.Selda.Selectors (unsafeSelector)
import Database.Selda.SqlType
import Data.Text (Text)
import Data.Proxy
import Unsafe.Coerce

-- | Cast a column to another type, using whichever coercion semantics are used
--   by the underlying SQL implementation.
cast :: forall s a b. SqlType b => Col s a -> Col s b
cast = liftC $ Cast (sqlType (Proxy :: Proxy b))

-- | Cast an aggregate to another type, using whichever coercion semantics
--   are used by the underlying SQL implementation.
castAggr :: forall s a b. SqlType b => Aggr s a -> Aggr s b
castAggr = liftAggr cast

-- | Sink the given function into an inner scope.
--
--   Be careful not to use this function with functions capturing rows or columns
--   from an outer scope. For instance, the following usage will likely
--   lead to disaster:
--
-- > query $ do
-- >   x <- #age `from` select person
-- >   inner $ sink (\p -> x + (p ! #age)) <$> select person
--
--   Really, if you have to use this function, ONLY do so in the global scope.
sink :: (f s a -> f s b) -> f (Inner s) a -> f (Inner s) b
sink = unsafeCoerce

-- | Like 'sink', but with two arguments.
sink2 :: (f s a -> f s b -> f s c) -> f (Inner s) a -> f (Inner s) b -> f (Inner s) c
sink2 = unsafeCoerce

-- | A unary operation. Note that the provided function name is spliced
--   directly into the resulting SQL query. Thus, this function should ONLY
--   be used to implement well-defined functions that are missing from Selda's
--   standard library, and NOT in an ad hoc manner during queries.
fun :: Text -> Col s a -> Col s b
fun = liftC . UnOp . Fun

-- | Like 'fun', but with two arguments.
fun2 :: Text -> Col s a -> Col s b -> Col s c
fun2 = liftC2 . Fun2

-- | A custom operator. @operator "~>" a b@ will compile down to
--   @a ~> b@, with parentheses around @a@ and @b@ iff they are not atomic.
--   This means that SQL operator precedence is disregarded, as all
--   subexpressions are parenthesized. In the following example for instance,
--   @foo a b c@ will compile down to @(a ~> b) ~> c@.
--
-- > (~>) = operator "~>"
-- > infixl 5 ~>
-- > foo a b c = a ~> b ~> c
operator :: Text -> Col s a -> Col s b -> Col s c
operator = liftC2 . BinOp . CustomOp

-- | Like 'fun', but with zero arguments.
fun0 :: Text -> Col s a
fun0 = One . NulOp . Fun0
