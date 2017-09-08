{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators, GADTs, FlexibleContexts #-}
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
--   The following example shows off Selda's most basic features -- creating,
--   populating, modifying and querying tables -- and is intended to act as a
--   Hello World-ish quickstart.
--
-- > {-# LANGUAGE TypeOperators, OverloadedStrings #-}
-- > import Data.Text (Text, unpack)
-- > import Database.Selda
-- > import Database.Selda.SQLite
-- >
-- > people :: Table (Text :*: Int :*: Maybe Text)
-- > (people, pName :*: pAge :*: pPet)
-- >   = tableWithSelectors "people"
-- >   $   primary "name"
-- >   :*: required "age"
-- >   :*: optional "pet"
-- >
-- > main = withSQLite "people.sqlite" $ do
-- >   createTable people
-- >
-- >   insert_ people
-- >     [ "Velvet"    :*: 19 :*: Nothing
-- >     , "Kobayashi" :*: 23 :*: Just "dragon"
-- >     , "Miyu"      :*: 10 :*: Nothing
-- >     ]
-- >
-- >   update_ people
-- >     (\person -> person ! pName .== "Velvet")
-- >     (\person -> person `with` [pPet := just "orthros"])
-- >
-- >   adults <- query $ do
-- >     person <- select people
-- >     restrict (person ! pAge .> 20)
-- >     return (person ! pName :*: person ! pAge)
-- >
-- >   n <- deleteFrom people (\person -> isNull (person ! pPet))
-- >
-- >   liftIO $ do
-- >     putStrLn "The adults in the room are:"
-- >     mapM_ printPerson adults
-- >     putStrLn $ show n ++ " people were deleted for having no pets."
-- >
-- > printPerson :: Text :*: Int -> IO ()
-- > printPerson (name :*: age) = putStrLn $ unpack name ++ ", age " ++ show age
--
--   Please see <http://hackage.haskell.org/package/selda/#readme>
--   for a more comprehensive tutorial.
module Database.Selda
  ( -- * Running queries
    MonadSelda
  , SeldaError (..), ValidationError
  , SeldaT, SeldaM, Table, Query, Col, Res, Result
  , query, transaction, setLocalCache
    -- * Constructing queries
  , Selector, (!), Assignment(..), with
  , SqlType (..)
  , Cols, Columns
  , Order (..)
  , (:*:)(..)
  , select, selectValues, from, distinct
  , restrict, limit
  , order , ascending, descending
  , inner, suchThat
    -- * Expressions over columns
  , Set (..)
  , RowID, invalidRowId, isInvalidRowId, fromRowId
  , (.==), (./=), (.>), (.<), (.>=), (.<=), like
  , (.&&), (.||), not_
  , literal, int, float, text, true, false, null_
  , roundTo, length_, isNull, ifThenElse, matchNull
    -- * Converting between column types
  , round_, just, fromBool, fromInt, toString
    -- * Inner queries
  , Aggr, Aggregates, OuterCols, LeftCols, Inner, SqlOrd
  , innerJoin, leftJoin
  , aggregate, groupBy
  , count, avg, sum_, max_, min_
    -- * Modifying tables
  , Insert
  , insert, insert_, insertWithPK, tryInsert, insertUnless, insertWhen, def
  , update, update_, upsert
  , deleteFrom, deleteFrom_
    -- * Prepared statements
  , Preparable, Prepare
  , prepared
    -- * Defining schemas
  , TableSpec, ColSpecs, ColSpec, TableName, ColName
  , NonNull, IsNullable, Nullable, NotNullable
  , Append (..), (:++:)
  , Selectors, HasSelectors
  , table, tableWithSelectors, selectors
  , required, optional
  , primary, autoPrimary
  , fk, unique
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
import Database.Selda.Backend
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.Frontend
import Database.Selda.Inner
import Database.Selda.Prepared
import Database.Selda.Query
import Database.Selda.Query.Type
import Database.Selda.Selectors
import Database.Selda.SQL hiding (distinct)
import Database.Selda.SqlType
import Database.Selda.Table
import Database.Selda.Table.Compile
import Database.Selda.Table.Foreign
import Database.Selda.Types
import Database.Selda.Unsafe
import Control.Exception (throw)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Typeable (eqT, (:~:)(..))
import Unsafe.Coerce

-- | Any column type that can be used with the 'min_' and 'max_' functions.
class SqlType a => SqlOrd a
instance {-# OVERLAPPABLE #-} (SqlType a, Num a) => SqlOrd a
instance SqlOrd Text
instance SqlOrd Day
instance SqlOrd UTCTime
instance SqlOrd TimeOfDay
instance SqlOrd a => SqlOrd (Maybe a)

-- | Convenient shorthand for @fmap (! sel) q@.
--   The following two queries are quivalent:
--
-- > q1 = name `from` select people
-- > q2 = do
-- >   person <- select people
-- >   return (person ! name)
from :: ToDyn (Cols () a)
     => Selector a b
     -> Query s (Cols s a)
     -> Query s (Col s b)
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
isNull :: Col s (Maybe a) -> Col s Bool
isNull = liftC $ UnOp IsNull

-- | Applies the given function to the given nullable column where it isn't null,
--   and returns the given default value where it is.
--
--   This is the Selda equivalent of 'maybe'.
matchNull :: SqlType a => Col s b -> (Col s a -> Col s b) -> Col s (Maybe a) -> Col s b
matchNull nullvalue f x = ifThenElse (isNull x) nullvalue (f (cast x))

-- | Any container type for which we can check object membership.
class Set set where
  -- | Is the given column contained in the given set?
  isIn :: SqlType a => Col s a -> set (Col s a) -> Col s Bool
infixl 4 `isIn`

instance Set [] where
  -- TODO: use safe coercions instead of unsafeCoerce
  isIn _ []     = false
  isIn (C x) xs = C $ InList x (unsafeCoerce xs)

instance Set (Query s) where
  isIn (C x) = C . InQuery x . snd . compQueryWithFreshScope

(.&&), (.||) :: Col s Bool -> Col s Bool -> Col s Bool
(.&&) = liftC2 $ BinOp And
(.||) = liftC2 $ BinOp Or
infixr 3 .&&
infixr 2 .||

-- | Ordering for 'order'.
ascending, descending :: Order
ascending = Asc
descending = Desc

-- | The default value for a column during insertion.
--   For an auto-incrementing primary key, the default value is the next key.
--
--   Using @def@ in any other context than insertion results in a runtime error.
def :: SqlType a => a
def = throw DefaultValueException

-- | Lift a non-nullable column to a nullable one.
--   Useful for creating expressions over optional columns:
--
-- > people :: Table (Text :*: Int :*: Maybe Text)
-- > people = table "people" $ required "name" :*: required "age" :*: optional "pet"
-- >
-- > peopleWithCats = do
-- >   name :*: _ :*: pet <- select people
-- >   restrict (pet .== just "cat")
-- >   return name
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
min_  :: SqlOrd a => Col s a -> Aggr s a
min_ = aggr "MIN"

-- | Sum all values in the given column.
sum_ :: (SqlType a, Num a) => Col s a -> Aggr s a
sum_ = aggr "SUM"

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

-- | Convert any column to a string.
toString :: Col s a -> Col s Text
toString = cast

-- | Perform a conditional on a column
ifThenElse :: Col s Bool -> Col s a -> Col s a -> Col s a
ifThenElse = liftC3 If
