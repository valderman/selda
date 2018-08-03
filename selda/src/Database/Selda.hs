{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators, GADTs, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies, CPP #-}
{-# LANGUAGE DataKinds #-}
-- | Selda is not LINQ, but they're definitely related.
--
--   Selda is a high-level EDSL for interacting with relational databases.
--   All database computations are performed within some monad implementing
--   the 'MonadSelda' type class. The 'SeldaT' monad over any @MonadIO@ is the
--   only pre-defined instance of @MonadSelda@.
--   'SeldaM' is provided as a convenient short-hand for @SeldaT IO@.
--
--   To actually execute a database computation, you need one of the database
--   backends: @selda-sqlite@ or @selda-postgresql@.
--
--   All Selda functions may throw 'SeldaError' when something goes wrong.
--   This includes database connection errors, uniqueness constraint errors,
--   etc.
--
--   See <https://selda.link/tutorial> for a tutorial covering the language
--   basics.
module Database.Selda
  ( -- * Running queries
    MonadSelda
  , SeldaError (..), ValidationError
  , SeldaT, SeldaM
  , Relational, Only (..), The (..)
  , Table, Query, Col, Res, Result
  , query, queryInto
  , transaction, setLocalCache, withoutForeignKeyEnforcement
    -- * Constructing queries
  , Selector, Source, Selected, (!), Assignment ((:=)), with
  , (+=), (-=), (*=), (||=), (&&=), ($=)
  , SqlType (..), SqlResult (..), SqlEnum (..)
  , Cols, Columns
  , Order (..)
  , (:*:)(..)
  , select, selectValues, from, distinct
  , restrict, limit
  , order, ascending, descending
  , orderRandom
  , inner, suchThat
    -- * Expressions over columns
  , Set (..)
  , ID, invalidId, isInvalidId, untyped, toId
  , RowID, invalidRowId, isInvalidRowId, fromRowId, toRowId
  , (.==), (./=), (.>), (.<), (.>=), (.<=), like
  , (.&&), (.||), not_
  , literal, int, float, text, true, false, null_
  , roundTo, length_, isNull, ifThenElse, matchNull
  , new, only
    -- * Converting between column types
  , round_, just, fromBool, fromInt, toString
    -- * Inner queries
  , Aggr, Aggregates, OuterCols, AggrCols, LeftCols, Inner, SqlOrd
  , innerJoin, leftJoin
  , aggregate, groupBy
  , count, avg, sum_, max_, min_
    -- * Modifying tables
  , insert, insert_, insertWithPK, tryInsert, insertUnless, insertWhen, def
  , update, update_, upsert
  , deleteFrom, deleteFrom_
    -- * Prepared statements
  , Preparable, Prepare
  , prepared
    -- * Defining schemas
  , Generic
  , TableName, ColName, Attr (..), Attribute
  , Selectors, ForeignKey (..)
  , table, tableFieldMod, tableWithSelectors, selectors
  , primary, autoPrimary, untypedAutoPrimary, unique
  , IndexMethod (..), index, indexUsing
    -- * Creating and dropping tables
  , createTable, tryCreateTable
  , dropTable, tryDropTable
    -- * Compiling and inspecting queries
  , OnError (..)
  , compile
  , compileCreateTable, compileDropTable
  , compileInsert, compileUpdate
    -- * Tuple convenience functions
  , Tup, Head
  , first, second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth
    -- * Useful re-exports
  , MonadIO, liftIO
  , Text, Day, TimeOfDay, UTCTime
  ) where
import Data.Typeable (Typeable)
import Database.Selda.Backend
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.Frontend
import Database.Selda.Generic
import Database.Selda.Inner
import Database.Selda.Prepared
import Database.Selda.Query
import Database.Selda.Query.Type
import Database.Selda.Selectors
import Database.Selda.SQL hiding (distinct)
import Database.Selda.SqlResult
import Database.Selda.SqlType
import Database.Selda.Table
import Database.Selda.Table.Compile
import Database.Selda.Table.Validation
import Database.Selda.Types
import Database.Selda.Unsafe
import Data.Proxy
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Typeable (eqT, (:~:)(..))
import GHC.Generics (Rep)
import Unsafe.Coerce

#if MIN_VERSION_base(4, 9, 0)
import GHC.TypeLits as TL
#endif

-- | Any column type that can be used with the 'min_' and 'max_' functions.
class SqlType a => SqlOrd a
instance {-# OVERLAPPABLE #-} (SqlType a, Num a) => SqlOrd a
instance SqlOrd RowID
instance SqlOrd Text
instance SqlOrd Day
instance SqlOrd UTCTime
instance SqlOrd TimeOfDay
instance SqlOrd a => SqlOrd (Maybe a)
instance Typeable a => SqlOrd (ID a)

-- | Wrapper for single column tables.
--   Use this when you need a table with only a single column, with 'table' or
--   'selectValues'.
newtype Only a = Only a
  deriving
    ( Generic
    , Show
    , Read
    , Eq
    , Ord
    , Enum
    , Num
    , Integral
    , Fractional
    , Real
    , IsString
    )
instance SqlType a => SqlResult (Only a)

#if MIN_VERSION_base(4, 9, 0)
instance (TypeError
  ( 'TL.Text "'Only " ':<>: 'ShowType a ':<>: 'TL.Text "' is not a proper SQL type."
    ':$$: 'TL.Text "Use 'the' to access the value of the column."
  ), Typeable a) => SqlType (Only a) where
  mkLit = error "unreachable"
  sqlType = error "unreachable"
  fromSql = error "unreachable"
  defaultValue = error "unreachable"
#endif

-- | Add the given column to the column pointed to by the given selector.
(+=) :: (SqlType a, Num (Col s a)) => Selector t a -> Col s a -> Assignment s t
s += c = s $= (+ c)
infixl 2 +=

-- | Subtract the given column from the column pointed to by the given selector.
(-=) :: (SqlType a, Num (Col s a)) => Selector t a -> Col s a -> Assignment s t
s -= c = s $= (\x -> x - c)
infixl 2 -=

-- | Multiply the column pointed to by the given selector, by the given column.
(*=) :: (SqlType a, Num (Col s a)) => Selector t a -> Col s a -> Assignment s t
s *= c = s $= (* c)
infixl 2 *=

-- | Logically @OR@ the column pointed to by the given selector with
--   the given column.
(||=) :: Selector t Bool -> Col s Bool -> Assignment s t
s ||= c = s $= (.|| c)
infixl 2 ||=

-- | Logically @AND@ the column pointed to by the given selector with
--   the given column.
(&&=) :: Selector t Bool -> Col s Bool -> Assignment s t
s &&= c = s $= (.&& c)
infixl 2 &&=

class The a where
  type TheOnly a
  -- | Extract the value of a row from a singleton table.
  the :: a -> TheOnly a

instance The (Only a) where
  type TheOnly (Only a) = a
  the (Only x) = x

instance The (Col s (Only a)) where
  type TheOnly (Col s (Only a)) = Col s a
  the (Many [Untyped x]) = One (unsafeCoerce x)
  the (Many _)           = error "BUG: non-singleton Only-column"
  the (One _)            = error "BUG: Only-column with raw expression"

-- | Create a singleton table column from an appropriate value.
only :: SqlType a => Col s a -> Col s (Only a)
only (One x)  = Many [Untyped x]
only (Many _) = error "BUG: SqlType compound column"

-- | Create a new column with the given fields.
--   Any unassigned fields will contain their default values.
new :: forall s a. Relational a => [Assignment s a] -> Col s a
new fields = Many (gNew (Proxy :: Proxy (Rep a))) `with` fields

-- | Convenient shorthand for @fmap (! sel) q@.
--   The following two queries are quivalent:
--
-- > q1 = name `from` select people
-- > q2 = do
-- >   person <- select people
-- >   return (person ! name)
from :: (Typeable a, SqlType b)
     => Selector (Source a) b
     -> Query s (Col s a)
     -> Query s (Col s (Selected a b))
from s q = (! s) <$> q
infixr 7 `from`

-- | Explicitly create an inner query. Equivalent to @innerJoin (const true)@.
--
--   Sometimes it's handy, for performance
--   reasons and otherwise, to perform a subquery and restrict only that query
--   before adding the result of the query to the result set, instead of first
--   adding the query to the result set and restricting the whole result set
--   afterwards.
inner :: (Columns a, Columns (OuterCols a))
      => Query (Inner s) a
      -> Query s (OuterCols a)
inner = innerJoin (const true)

-- | Create and filter an inner query, before adding it to the current result
--   set.
--
--   @q `suchThat` p@ is generally more efficient than
--   @select q >>= \x -> restrict (p x) >> pure x@.
suchThat :: (Columns a, Columns (OuterCols a))
         => Query (Inner s) a
         -> (a -> Col (Inner s) Bool)
         -> Query s (OuterCols a)
suchThat q p = inner $ do
  x <- q
  restrict (p x)
  return x
infixr 7 `suchThat`

(.==), (./=) :: SqlType a => Col s a -> Col s a -> Col s Bool
(.>), (.<), (.>=), (.<=) :: SqlOrd a => Col s a -> Col s a -> Col s Bool
(.==) = liftC2 $ BinOp Eq
(./=) = liftC2 $ BinOp Neq
(.>)  = liftC2 $ BinOp Gt
(.<)  = liftC2 $ BinOp Lt
(.>=) = liftC2 $ BinOp Gte
(.<=) = liftC2 $ BinOp Lte
infixl 4 .==
infixl 4 ./=
infixl 4 .>
infixl 4 .<
infixl 4 .>=
infixl 4 .<=

-- | Is the given column null?
isNull :: SqlType a => Col s (Maybe a) -> Col s Bool
isNull = liftC $ UnOp IsNull

-- | Applies the given function to the given nullable column where it isn't null,
--   and returns the given default value where it is.
--
--   This is the Selda equivalent of 'maybe'.
matchNull :: (SqlType a, SqlType b)
          => Col s b
          -> (Col s a -> Col s b)
          -> Col s (Maybe a)
          -> Col s b
matchNull nullvalue f x = ifThenElse (isNull x) nullvalue (f (cast x))

-- | Any container type for which we can check object membership.
class Set set where
  -- | Is the given column contained in the given set?
  isIn :: (SqlType a, SqlResult a) => Col s a -> set (Col s a) -> Col s Bool
infixl 4 `isIn`

instance Set [] where
  isIn _ []     = false
  isIn (One x) xs = One $ InList x [c | One c <- xs]
  isIn (Many _) _ = error "unreachable"

instance Set (Query s) where
  isIn (One x) = One . InQuery x . snd . compQueryWithFreshScope
  isIn (Many _) = error "unreachable"

(.&&), (.||) :: Col s Bool -> Col s Bool -> Col s Bool
(.&&) = liftC2 $ BinOp And
(.||) = liftC2 $ BinOp Or
infixr 3 .&&
infixr 2 .||

-- | Ordering for 'order'.
ascending, descending :: Order
ascending = Asc
descending = Desc

-- | Lift a non-nullable column to a nullable one.
--   Useful for creating expressions over optional columns:
--
-- > data Person = Person {name :: Text, age :: Int, pet :: Maybe Text}
-- >   deriving Generic
-- > instance SqlResult Person
-- >
-- > people :: Table Person
-- > people = table "people" []
-- > sName :*: sAge :*: sPet = selectors people
-- >
-- > peopleWithCats = do
-- >   person <- select people
-- >   restrict (person ! sPet .== just "cat")
-- >   return (name ! sName)
just :: SqlType a => Col s a -> Col s (Maybe a)
just = cast

-- | SQL NULL, at any type you like.
null_ :: SqlType a => Col s (Maybe a)
null_ = literal Nothing

-- | Specialization of 'literal' for integers.
int :: Int -> Col s Int
int = literal

-- | Specialization of 'literal' for doubles.
float :: Double -> Col s Double
float = literal

-- | Specialization of 'literal' for text.
text :: Text -> Col s Text
text = literal

-- | True and false boolean literals.
true, false :: Col s Bool
true = literal True
false = literal False

-- | The SQL @LIKE@ operator; matches strings with @%@ wildcards.
--   For instance:
--
-- > "%gon" `like` "dragon" .== true
like :: Col s Text -> Col s Text -> Col s Bool
like = liftC2 $ BinOp Like
infixl 4 `like`

-- | The number of non-null values in the given column.
count :: SqlType a => Col s a -> Aggr s Int
count = aggr "COUNT"

-- | The average of all values in the given column.
avg :: (SqlType a, Num a) => Col s a -> Aggr s a
avg = aggr "AVG"

-- | The greatest value in the given column. Texts are compared lexically.
max_ :: SqlOrd a => Col s a -> Aggr s a
max_ = aggr "MAX"

-- | The smallest value in the given column. Texts are compared lexically.
min_ :: SqlOrd a => Col s a -> Aggr s a
min_ = aggr "MIN"

-- | Sum all values in the given column.
sum_ :: forall a b s. (SqlType a, SqlType b, Num a, Num b) => Col s a -> Aggr s b
sum_ = castAggr . aggr "SUM"

-- | Round a value to the nearest integer. Equivalent to @roundTo 0@.
round_ :: forall s a. (SqlType a, Num a) => Col s Double -> Col s a
round_ =
  case eqT :: Maybe (a :~: Double) of
    Just Refl -> fun "ROUND"
    _         -> cast . fun "ROUND"

-- | Round a column to the given number of decimals places.
roundTo :: Col s Int -> Col s Double -> Col s Double
roundTo = flip $ fun2 "ROUND"

-- | Calculate the length of a string column.
length_ :: Col s Text -> Col s Int
length_ = fun "LENGTH"

-- | Boolean negation.
not_ :: Col s Bool -> Col s Bool
not_ = liftC $ UnOp Not

-- | Convert a boolean column to any numeric type.
fromBool :: (SqlType a, Num a) => Col s Bool -> Col s a
fromBool = cast

-- | Convert an integer column to any numeric type.
fromInt :: (SqlType a, Num a) => Col s Int -> Col s a
fromInt = cast

-- | Convert any SQL type to a string.
toString :: SqlType a => Col s a -> Col s Text
toString = cast

-- | Perform a conditional on a column
ifThenElse :: SqlType a => Col s Bool -> Col s a -> Col s a -> Col s a
ifThenElse = liftC3 If
