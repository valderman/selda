{-# LANGUAGE OverloadedStrings #-}
-- | Selda is not LINQ, but they're definitely related.
--
--   Selda is a high-level EDSL for interacting with relational databases.
module Database.Selda
  ( -- * Running queries
    SeldaT, Table, Query, Col, Res, MonadIO (..), MonadTrans (..), Result
  , query
    -- * Constructing queries
  , SqlType
  , Text, Cols, Columns
  , Order (..)
  , (:*:)(..)
  , select, restrict, limit, order
  , ascending, descending
    -- * Expressions over columns
  , (.==), (./=), (.>), (.<), (.>=), (.<=), (.&&), (.||), is, isn't, like
  , literal, int, float, text, true, false, not_, round_, roundTo, length_
  , just, null_
    -- * Inner queries
  , Aggr, Aggregates, OuterCols, JoinCols, Inner
  , leftJoin
  , aggregate, groupBy
  , some, count, avg, sum_, max_, min_
    -- * Modifying tables
  , Insert
  , insert, insert_
  , update, update_
  , deleteFrom, deleteFrom_
    -- * Defining schemas
  , ColSpec, TableName, ColName
  , NonNull, IsNullable, Nullable, NotNullable
  , table, (¤), required, optional
  , primary, autoIncrement
    -- * Combining schemas
  , ComposeSpec, (:+++:), (+++)
    -- * Creating and dropping tables
  , createTable, tryCreateTable
  , dropTable, tryDropTable
    -- * Compiling and inspecting queries
  , OnError (..)
  , compile
  , compileCreateTable, compileDropTable
  , compileInsert, compileUpdate
    -- * Unsafe functions for dialect-specific extensions
  , fun, fun2, cast
  ) where
import Data.Text (Text)
import Database.Selda.Aggregates
import Database.Selda.Backend
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.Query
import Database.Selda.Query.Type
import Database.Selda.SQL (Order (..))
import Database.Selda.SqlType
import Database.Selda.Table
import Database.Selda.Table.Compile
import Database.Selda.Types

(.==), (./=), (.>), (.<), (.>=), (.<=), is, isn't :: Col s a -> Col s a -> Col s Bool
(.==) = liftC2 $ BinOp Eq
(./=) = liftC2 $ BinOp Neq
(.>)  = liftC2 $ BinOp Gt
(.<)  = liftC2 $ BinOp Lt
(.>=) = liftC2 $ BinOp Gte
(.<=) = liftC2 $ BinOp Lte
is    = liftC2 $ BinOp Is
isn't = liftC2 $ BinOp IsNot
infixl 4 .==
infixl 4 .>
infixl 4 .<
infixl 4 .>=
infixl 4 .<=
infixl 4 `is`
infixl 4 `isn't`

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
-- > people :: Table (Text :*: Int :*: Maybe Text)
-- > people = table "people" $ required "name" ¤ required "age" ¤ optional "pet"
-- >
-- > peopleWithCats = do
-- >   name :*: _ :*: pet <- select people
-- >   restrict (pet .== just "cat")
-- >   return name
just :: Col s a -> Col s (Maybe a)
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

-- | The SQL @LIKE@ operator; matches strings with wildcards.
like :: Col s Text -> Col s Text -> Col s Bool
like = liftC2 $ BinOp Like
infixl 4 `like`

-- | Cast a column to another type, using whichever coercion semantics are used
--   by the underlying SQL implementation.
cast :: Col s a -> Col s b
cast = liftC Cast

-- | The number of non-null values in the given column.
count :: Col s a -> Aggr s Int
count = aggr "COUNT"

-- | Choose an arbitrary value from the given column.
--   This is useful for queries like
--   @SELECT COUNT(address), address FROM addresses GROUP BY address@,
--   where you want a representative of a column on an aggregated query, but
--   you don't particularly care which one.
some :: Col s a -> Aggr s a
some = aggr ""

-- | The average of all values in the given column.
avg :: Num a => Col s a -> Aggr s a
avg = aggr "AVG"

-- | The greatest value in the given column. Texts are compared lexically.
max_ :: Col s a -> Aggr s a
max_ = aggr "MAX"

-- | The smallest value in the given column. Texts are compared lexically.
min_  :: Col s a -> Aggr s a
min_ = aggr "MIN"

-- | Sum all values in the given column.
sum_ :: Num a => Col s a -> Aggr s a
sum_ = aggr "SUM"

-- | Round a value to the nearest integer. Equivalent to @roundTo 0@.
round_ :: Num a => Col s a -> Col s a
round_ = fun "ROUND"

-- | Round a column to the given number of decimals places.
roundTo :: Num a => Col s Int -> Col s a -> Col s a
roundTo = flip $ fun2 "ROUND"

-- | Calculate the length of a string column.
length_ :: Col s Text -> Col s Int
length_ = fun "LENGTH"

-- | Boolean negation.
not_ :: Col s Bool -> Col s Bool
not_ = liftC $ UnOp Not
