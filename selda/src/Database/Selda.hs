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
    MonadSelda, Backend
  , SeldaError (..), ValidationError
  , SeldaT, SeldaM
  , Relational, Only (..), The (..)
  , Table (tableName), Query, Row, Col, Res, Result
  , query, queryInto
  , transaction, withoutForeignKeyEnforcement
  , newUuid

    -- * Constructing queries
  , SqlType (..), SqlRow (..), SqlEnum (..)
  , Columns, Same
  , Order (..)
  , (:*:)(..)
  , select, selectValues, from, distinct
  , restrict, limit
  , order, ascending, descending
  , orderRandom
  , inner, suchThat

    -- * Working with selectors
  , Selector, Coalesce
  , HasField, FieldType, IsLabel
  , (!), (?), Assignment ((:=)), with
  , (+=), (-=), (*=), (||=), (&&=), ($=)

    -- * Expressions over columns
  , Set (..)
  , ID, invalidId, isInvalidId, untyped, fromId, toId
  , RowID, invalidRowId, isInvalidRowId, fromRowId, toRowId
  , (.==), (./=), (.>), (.<), (.>=), (.<=), like
  , (.&&), (.||), not_
  , literal, is, int, float, text, true, false, null_
  , roundTo, length_, isNull, ifThenElse, ifNull, matchNull
  , new, row, only
  , Mappable (..)

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
  , ForeignKey (..)
  , SelectorLike, Group (..), sel
  , table, tableFieldMod
  , primary, autoPrimary, weakAutoPrimary
  , untypedAutoPrimary, weakUntypedAutoPrimary
  , unique
  , IndexMethod (..), index, indexUsing

    -- * Creating and dropping tables
  , createTable, tryCreateTable
  , dropTable, tryDropTable

    -- * Tuple convenience functions
  , Tup, Head
  , first, second, third, fourth, fifth

    -- * Useful re-exports
  , MonadIO, MonadMask, liftIO
  , Text, Day, TimeOfDay, UTCTime, UUID
  ) where
import Control.Monad.Catch (MonadMask)
import Data.Typeable (Typeable)
import Database.Selda.Backend
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.FieldSelectors
import Database.Selda.Frontend
import Database.Selda.Generic
import Database.Selda.Inner
import Database.Selda.Prepared
import Database.Selda.Query
import Database.Selda.Query.Type
import Database.Selda.Selectors
import Database.Selda.SQL hiding (distinct)
import Database.Selda.SqlRow
import Database.Selda.Table
import Database.Selda.Table.Validation
import Database.Selda.Types
import Database.Selda.Unsafe
import Data.Proxy
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Typeable (eqT, (:~:)(..))
import GHC.Generics (Rep)
import qualified GHC.Generics as G (from)
import Unsafe.Coerce
import System.Random (randomIO)
import GHC.TypeLits as TL

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
instance SqlType a => SqlRow (Only a)

instance (TypeError
  ( 'TL.Text "'Only " ':<>: 'ShowType a ':<>: 'TL.Text "' is not a proper SQL type."
    ':$$: 'TL.Text "Use 'the' to access the value of the column."
  ), Typeable a) => SqlType (Only a) where
  mkLit = error "unreachable"
  sqlType = error "unreachable"
  fromSql = error "unreachable"
  defaultValue = error "unreachable"

-- | Generate a new random UUID using the system's random number generator.
--   UUIDs generated this way are (astronomically likely to be) unique,
--   but not necessarily unpredictable.
--
--   For applications where unpredictability is crucial, take care to use a
--   proper cryptographic PRNG to generate your UUIDs.
newUuid :: MonadIO m => m UUID
newUuid = liftIO randomIO

-- | Annotation to force the type of a polymorphic label (i.e. @#foo@) to
--   be a selector. This is useful, for instance, when defining unique
--   constraints: @sel #foo :- unique@.
sel :: Selector t a -> Selector t a
sel = id

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

instance The (Row s (Only a)) where
  type TheOnly (Row s (Only a)) = Col s a
  the (Many [Untyped x]) = One (unsafeCoerce x)
  the (Many _)           = error "BUG: non-singleton Only-column"

-- | Create a singleton table column from an appropriate value.
only :: SqlType a => Col s a -> Row s (Only a)
only (One x)  = Many [Untyped x]

-- | Create a new row with the given fields.
--   Any unassigned fields will contain their default values.
new :: forall s a. Relational a => [Assignment s a] -> Row s a
new fields = Many (gNew (Proxy :: Proxy (Rep a))) `with` fields

-- | Create a new row from the given value.
--   This can be useful when you want to update all or most of a row:
--
-- > update users (#uid `is` user_id)
-- >              (\old -> row user_info `with` [...])
row :: forall s a. Relational a => a -> Row s a
row x = Many (gRow (G.from x))

-- | Convenient shorthand for @fmap (! sel) q@.
--   The following two queries are quivalent:
--
-- > q1 = name `from` select people
-- > q2 = do
-- >   person <- select people
-- >   return (person ! name)
from :: (Typeable t, SqlType a)
     => Selector t a
     -> Query s (Row s t)
     -> Query s (Col s a)
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

-- | Comparisons over columns.
--   Note that when comparing nullable (i.e. @Maybe@) columns, SQL @NULL@
--   semantics are used. This means that comparing to a @NULL@ field will remove
--   the row in question from the current set.
--   To test for @NULL@, use 'isNull' instead of @.== literal Nothing@.
(.==), (./=) :: (Same s t, SqlType a) => Col s a -> Col t a -> Col s Bool
(.>), (.<), (.>=), (.<=) :: (Same s t, SqlOrd a) => Col s a -> Col t a -> Col s Bool
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
matchNull :: (SqlType a, SqlType b, Same s t)
          => Col s b
          -> (Col s a -> Col s b)
          -> Col t (Maybe a)
          -> Col s b
matchNull nullvalue f x = ifThenElse (isNull x) nullvalue (f (cast x))

-- | If the second value is Nothing, return the first value. Otherwise return
--   the second value.
ifNull :: (Same s t, SqlType a) => Col s a -> Col t (Maybe a) -> Col s a
ifNull nullvalue x = ifThenElse (isNull x) nullvalue (cast x)

-- | Any container type which can be mapped over.
--   Sort of like 'Functor', if you squint a bit.
class Mappable f where
  type Container f a
  (.<$>) :: (SqlType a, SqlType b)
         => (Col s a -> Col s b)
         -> f s (Container f a)
         -> f s (Container f b)
infixl 4 .<$>

instance Mappable Aggr where
  type Container Aggr a = a
  (.<$>) = liftAggr

instance Mappable Col where
  type Container Col a = Maybe a
  f .<$> mx = cast (f (cast mx))

-- | Any container type for which we can check object membership.
class Set set where
  -- | Is the given column contained in the given set?
  isIn :: (Same s t, SqlType a) => Col s a -> set (Col t a) -> Col s Bool
infixl 4 `isIn`

instance Set [] where
  isIn _ []     = false
  isIn (One x) xs = One $ InList x [c | One c <- xs]

instance Set (Query s) where
  isIn (One x) = One . InQuery x . snd . compQueryWithFreshScope

(.&&), (.||) :: Same s t => Col s Bool -> Col t Bool -> Col s Bool
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
-- > instance SqlRow Person
-- >
-- > people :: Table Person
-- > people = table "people" []
-- >
-- > peopleWithCats = do
-- >   person <- select people
-- >   restrict (person ! #pet .== just "cat")
-- >   return (person ! #name)
just :: SqlType a => Col s a -> Col s (Maybe a)
just = cast

-- | Returns 'true' if the given field in the given row is equal to the given
--   literal.
is :: forall r s c. SqlType c => Selector r c -> c -> Row s r -> Col s Bool
is s x r = r ! s .== (literal x :: Col s c)

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
like :: Same s t => Col s Text -> Col t Text -> Col s Bool
like = liftC2 $ BinOp Like
infixl 4 `like`

-- | The number of non-null values in the given column.
count :: SqlType a => Col s a -> Aggr s Int
count = aggr "COUNT"

-- | The average of all values in the given column.
avg :: (SqlType a, Num a) => Col s a -> Aggr s (Maybe a)
avg = aggr "AVG"

-- | The greatest value in the given column. Texts are compared lexically.
max_ :: SqlOrd a => Col s a -> Aggr s (Maybe a)
max_ = aggr "MAX"

-- | The smallest value in the given column. Texts are compared lexically.
min_ :: SqlOrd a => Col s a -> Aggr s (Maybe a)
min_ = aggr "MIN"

-- | Sum all values in the given column.
sum_ :: forall a b s. (SqlType a, SqlType b, Num a, Num b) => Col s a -> Aggr s b
sum_ = liftAggr (ifNull (0::Col s b) . cast) . aggr "SUM"

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
ifThenElse :: (Same s t, Same t u, SqlType a) => Col s Bool -> Col t a -> Col u a -> Col s a
ifThenElse = liftC3 If
