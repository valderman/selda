# Chapter 1: An Example, Explained

As Selda uses a lot of fancy type magic to achieve a safe yet flexible
programming model, figuring out how to use it from type signatures alone
can be hard. While the types keep queries nice and safe, the type errors
when something goes wrong can be a bit daunting.
Therefore it's especially important to get an intuition for how the language
works.

To start building this intuition, let's dissect the example from
the [front page](/) of the website, and use it to illustrate the core concepts
of the Selda library.

```
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Database.Selda
import Database.Selda.SQLite
```

We start off by declaring the language extensions we'll need to use, and
importing the core Selda library and the SQLite backend.
`Database.Selda` houses the Selda DSL itself, whereas
`Database.Selda.SQLite` allows us to run Selda programs over SQLite databases.

The `DeriveGeneric` extension lets us derive the `Generic` type class, which
is important since Selda uses generics heavily to map Haskell types to database
tables. `OverloadedStrings` is not strictly required, but highly recommended
to reduce boilerplate when working with `Text` fields.

```
data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet
```

Here, we declare a `Pet` *enumeration type*, to represent the various types
of pets one might have. Selda is able to map enumeration types - any type
implementing `Enum` and `Bounded` - if we just add an `SqlType` instance
for it. In this case, we also derive `Show` and `Read` which automatically
gives us the appropriate conversions between our `Pet` type and its database
representation.

The `SqlType` type class denotes types which are representable as a single
database column. These types include various numeric types, `Text`,
date and time types, etc.

```
data Person = Person
  { name :: Text
  , age :: Int
  , pet :: Maybe Pet
  } deriving Generic
instance SqlRow Person
```

Now it's time to create a data type to describe our table.
Any product type - a type with a single data constructor - where all fields
are instances of `SqlType` can be used for tables and as results from queries
if they implement the `SqlRow` type class.
By deriving `Generic` for our type, we can use the default `SqlRow` instance.
In fact, you should never need to implement `SqlRow` yourself.

```
people :: Table Person
people = table "people" [name :- primary]
```

Once we have the `Person` type and its `SqlRow` instance, building a table
from it is easy.
The `table` function accepts a table name, and a list of column attributes where
things like indexes, foreign keys and other constraints can be declared.
The record selectors of the table type are used to specify these attributes.

In our case, we want to specify the `name` fields as the primary key of our
table.

```
(s_name :*: s_age :*: s_pet) = selectors people
```

To access the columns of a table within queries, we need to declare the
*selectors* of the table: essentially a Selda-specific complement to
the type's normal Haskell record selectors.
The `selectors` function returns an *inductive tuple* - one or more values
separated by the `:*:` data constructor - or all selectors corresponding
to its given type.

In our example, the `people` table has three columns since it maps to the
`People` type, so `selectors people` will give us back three selectors.

```
main = withSQLite "people.sqlite" $ do
  ...
```

To run a Selda program, we need to specify a database to run it over.
In our example, we use the SQLite database and specify `people.sqlite` as the
file in which our database will be stored.

Selda computations are written in some monad which implements the
`MonadSelda` type class. For most Selda programs - including our example - the
built-in `SeldaM` monad is just fine.
Unless you really want to, you'll never need to bother with implementing
a Selda monad of your own.

```
createTable people
insert_ people
  [ Person "Velvet"    19 (Just Dog)
  , Person "Kobayashi" 23 (Just Dragon)
  , Person "Miyu"      10 Nothing
  ]
```

Before we can store any data in a table, we need to create it.
The `createTable` function will attempt to create its given table, and throw
a `SeldaError` if the table already exists.

Once we've created our table, we use `insert_` to insert three rows into
our newly created table. Each row is represented by a plani Haskell value of
the table's type.

```
adultsAndTheirPets <- query $ do
  ...
```

The main meat of the Selda library is, of course, queries.
Within a Selda computation the database can be queried using the `query`
function, which takes a query computation as its input.

Queries are written in the `Query` monad, which is parameterised over a
*scope parameter* `s`.
This parameter ensures that queries are always well-scoped (i.e. do not access
columns which are not in scope).
While this parameter makes type errors more complicated, it is a necessary evil
as it lets us reconcile the differences in scoping between SQL and Haskell
in a safe and (more or less) elegant way.


```
person <- select people
```

The `select` function is used to draw data rows from a table.
In this example `person` has the type `Row s Person`, and represents a single
row from the `people` table.

```
restrict (person ! s_age .>= 18)
```

The columns of a row can be accessed using the table's selectors.
The syntax for this is `row ! selector`. The column thus obtained can then be
arbitrarily used in expressions.

In this example, we use the `restrict` function (roughly equivalent
to SQL `WHERE`) to filter out all persons who have a `s_age` lower than 18.

```
return (person ! s_name :*: person ! s_pet)
```

Once we're done fetching rows and filtering, we can return any number of rows
or columns, grouped together as an inductive tuple.
Whatever we return from a query will, upon execution, be converted to
the corresponding Haskell type and returned from the `query` call we just
returned from.

The rows from a query are returned as a list, which each element corresponding
to a single result row. For instance, a query of type `Query s (Row s Person)`
will return `[Person]` when executed, and a query of
type `Query s (Row s Person :*: Col s Int)` will return `[Person :*: Int]`.

In this particular example, the `s_name` field of the person table has type
`Text` and the `s_pet` field has type `Maybe Text`.
From this we can deduce that the query has type
`Query s (Col s Text :*: Col s (Maybe Text))`, meaning that the type returned
back to Haskell land will be `[Text :*: Maybe Text]`.

```
liftIO $ print adultsAndTheirPets
```

To verify that this whole example works as advertised, we print out the entire
result set of the query.
As `SeldaM` implements `MonadIO` we can just lift the standard `print` function
into the computation.

Since Miyu is the only person in the table who is under the age of 18,
the result set, when printed, should look like this:

```
["Velvet" :*: Just Dog,"Kobayashi" :*: Just Dragon]
```
