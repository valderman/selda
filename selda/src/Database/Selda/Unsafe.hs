{-# LANGUAGE ScopedTypeVariables #-}
-- | Unsafe operations giving the user unchecked low-level control over
--   the generated SQL.
module Database.Selda.Unsafe
  ( fun, fun2, fun0, operator
  , aggr
  , cast, castAggr, sink, sink2
  , unsafeSelector
  , QueryFragment, inj, injLit, rawName, rawExp, rawStm, rawQuery, rawQuery1
  ) where
import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.State.Strict
    ( MonadIO(liftIO), MonadState(put, get) )
import Database.Selda.Backend.Internal
    ( SqlType(mkLit, sqlType),
      MonadSelda,
      SeldaBackend(runStmt, ppConfig),
      SeldaError(UnsafeError),
      withBackend )
import Database.Selda.Column
    ( BinOp(CustomOp),
      UnOp(Fun),
      NulOp(Fun0),
      Exp(Col, Cast, UnOp, Fun2, BinOp, NulOp, Lit, Raw),
      UntypedCol(Untyped),
      SomeCol(Named),
      hideRenaming,
      Same(liftC2),
      Row(..),
      Col(..),
      liftC )
import Database.Selda.Inner (Inner, Aggr, aggr, liftAggr)
import Database.Selda.Selectors (unsafeSelector)
import Database.Selda.Query.Type (Query (..), sources, renameAll, rename)
import Database.Selda.SQL (QueryFragment (..), SqlSource (RawSql), sqlFrom)
import Database.Selda.SQL.Print (compRaw)
import Database.Selda.SqlRow (SqlRow (..))
import Database.Selda.Types (ColName)
import Data.Text (Text)
import Data.Proxy ( Proxy(..) )
import Unsafe.Coerce ( unsafeCoerce )

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

-- | Create a raw SQL query fragment from the given column.
inj :: Col s a -> QueryFragment
inj (One x) = RawExp x

-- | Create a raw SQL query fragment from the given value.
injLit :: SqlType a => a -> QueryFragment
injLit = RawExp . Lit . mkLit

-- | Create a column referring to a name of your choice.
--   Use this to refer to variables not exposed by Selda.
rawName :: SqlType a => ColName -> Col s a
rawName = One . Col

-- | Create an expression from the given text.
--   The expression will be inserted verbatim into your query, so you should
--   NEVER pass user-provided text to this function.
rawExp :: SqlType a => Text -> Col s a
rawExp = One . Raw

-- | Execute a raw SQL statement.
rawStm :: MonadSelda m => QueryFragment -> m ()
rawStm q = withBackend $ \b -> liftIO $ do
  void $ uncurry (runStmt b) $ compRaw (ppConfig b) q

-- | Execute a raw SQL statement, returning a row consisting of columns by the
--   given names.
--   Will fail if the number of names given does not match up with
--   the type of the returned row.
--   Will generate invalid SQL if the given names don't match up with the
--   column names in the given query.
rawQuery :: forall a s. SqlRow a => [ColName] -> QueryFragment -> Query s (Row s a)
rawQuery names q
  | length names /= nestedCols (Proxy :: Proxy a) = do
      let err = concat
            [ "rawQuery: return type has ", show (nestedCols (Proxy :: Proxy a))
            , " columns, but only ", show (length names), " names were given"
            ]
      throw (UnsafeError err)
  | otherwise = Query $ do
      rns <- renameAll [Untyped (Col name) | name <- names]
      st <- get
      put $ st { sources = sqlFrom rns (RawSql q) : sources st }
      return (Many (map hideRenaming rns))

-- | As 'rawQuery', but returns only a single column. Same warnings still apply.
rawQuery1 :: SqlType a => ColName -> QueryFragment -> Query s (Col s a)
rawQuery1 name q = Query $ do
  name' <- head <$> rename (Untyped (Col name))
  st <- get
  put $ st { sources = sqlFrom [name'] (RawSql q) : sources st }
  case name' of
    Named n _ -> return (One (Col n))
    _         -> error "BUG: renaming did not rename"
