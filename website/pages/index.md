<div class="pane" id="left">

## What is Selda?

Selda is an EDSL &mdash; an *embedded domain-specific language*
&mdash; for defining, querying and modifying relational databases
from Haskell.
The same type-safe Selda query can be executed unmodified on
either PostgreSQL or SQLite, making Selda ideal for prototyping
as well as for taking the step from prototype to real application.

Through its monadic interface, Selda supports writing queries in a
linear, natural style. The generated SQL code is guaranteed to be
correct, type-safe and executable on all supported backends.
With a minimalist approach to dependencies, Selda is lightweight
enough to be suitable for inclusion in libraries as well as full
applications.
All non-essential features are optional, either through configuration
flags or through separate add-on packages.

## Features
* Type-safe queries, inserts, updates and deletes
* Monadic query language with general inner queries
* Seamless prepared statements
* Typed migrations
* Upserts, transactions, indexes and constraints
* Minimal dependencies
* In-process caching
* Backends for PostgreSQL and SQLite
* ...and much more!

</div>


<div class="pane" id="right">

## Installation

```language-shell widecode
$ cabal update
$ cabal install selda-sqlite selda-postgresql
```

```language-shell narrowcode
$ cabal update
$ cabal install selda-sqlite
$ cabal install selda-postgresql
```

## By example

```language-haskell
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Database.Selda
import Database.Selda.SQLite

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { name :: Text
  , age :: Int
  , pet :: Maybe Pet
  } deriving Generic
instance SqlRow Person

people :: Table Person
people = table "people" [name :- primary]
(s_name :*: s_age :*: s_pet) = selectors people

main = withSQLite "people.sqlite" $ do
  createTable people
  insert_ people
    [ Person "Velvet"    19 (Just Dog)
    , Person "Kobayashi" 23 (Just Dragon)
    , Person "Miyu"      10 Nothing
    ]

  adultsAndTheirPets <- query $ do
    person <- select people
    restrict (person ! s_age .>= 18)
    return (person ! s_name :*: person ! s_pet)
  liftIO $ print adultsAndTheirPets
```
</div>
