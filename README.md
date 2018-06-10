Selda
=====

[![Join the chat at https://gitter.im/selda-hs/Lobby](https://badges.gitter.im/selda-hs/Lobby.svg)](https://gitter.im/selda-hs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Hackage](https://img.shields.io/hackage/v/selda.svg?style=flat)](http://hackage.haskell.org/package/selda)
[![IRC channel](https://img.shields.io/badge/IRC-%23selda-1e72ff.svg?style=flat)](https://www.irccloud.com/invite?channel=%23selda&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1)
![MIT License](http://img.shields.io/badge/license-MIT-brightgreen.svg)
[![Build Status](https://travis-ci.org/valderman/selda.svg?branch=master)](https://travis-ci.org/valderman/selda)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/selda.svg)


What is Selda?
==============
[Selda](https://selda.link) is a Haskell library for interacting with SQL-based relational databases.
It was inspired by [LINQ](https://en.wikipedia.org/wiki/Language_Integrated_Query) and
[Opaleye](http://hackage.haskell.org/package/opaleye).


Features
========

* Monadic interface.
* Portable: backends for SQLite and PostgreSQL.
* Generic: easy integration with your existing Haskell types.
* Creating, dropping and querying tables using type-safe database schemas.
* Typed query language with products, filtering, joins and aggregation.
* Inserting, updating and deleting rows from tables.
* Conditional insert/update.
* Transactions, uniqueness constraints and foreign keys.
* Seamless prepared statements.
* Configurable, automatic, consistent in-process caching of query results.
* Lightweight and modular: few dependencies, and non-essential features are
  optional or split into add-on packages.


Getting started
===============

Install the `selda` package from Hackage, as well as at least one of the
backends:

    $ cabal update
    $ cabal install selda selda-sqlite selda-postgresql

Then, read the tutorial.
The [API documentation](http://hackage.haskell.org/package/selda) will probably
also come in handy.


Requirements
============

Selda requires GHC 7.10+, as well as SQLite 3.7.11+ or PostgreSQL 9.6+.
To build the SQLite backend, you need a C compiler installed.
To build the PostgreSQL backend, you need the `libpq` development libraries
installed (`libpq-dev` on Debian-based Linux distributions).


A brief tutorial
================

Defining a schema
-----------------

To work productively with Selda, you will need to enable the `TypeOperators` and
`OverloadedStrings` extensions.

Table schemas are defined as the product of one or more columns, stitched
together using the `:*:` operator.
A table is parameterized over the types of its columns, with the column types
also separated by the `:*:` operator. This, by the way, is why you need
`TypeOperators`.

```haskell
people :: Table (Text :*: Int :*: Maybe Text)
people = table "people" $ primary "name" :*: required "age" :*: optional "pet"

addresses :: Table (Text :*: Text)
addresses = table "addresses" $ required "name" :*: required "city"
```

Columns may be either `required` or `optional`.
Although the SQL standard supports nullable primary keys, Selda primary keys
are always required.


Running queries
---------------

Selda operations are run in the `SeldaT` monad transformer, which can be layered
on top of any `MonadIO`. Throughout this tutorial, we will simply use the Selda
monad `SeldaM`, which is just a synonym for `SeldaT IO`.
`SeldaT` is entered using a backend-specific `withX` function. For instance,
the SQLite backend uses the `withSQLite` function:

```haskell
main :: IO ()
main = withSQLite "my_database.sqlite" $ do
  people <- getAllPeople
  liftIO (print people)

getAllPeople :: SeldaM [Text :*: Int :*: Maybe Text]
getAllPeople = query (select people)
```

This will open the `my_database.sqlite` database for the duration of the
computation. If the computation terminates normally, or if it raises an
exception, the database is automatically closed.

Note the somewhat weird return type of `getAllPeople`. In Selda, queries are
represented using *inductive tuples*: a list of values, separated
by the `:*:` operator, but where each element can have a different type.
You can think of them as tuples with a slightly different syntax.
In this example, `getAllPeople` having a return type of
`[Text :*: Int :*: Maybe Text]` means that it returns a list of "3-tuples",
where the three elements have the types `Text`, `Int` and `Maybe Text`
respectively.

You can pattern match on these values as you would on normal tuples:

```haskell
firstOfThree :: (a :*: b :*: c) -> a
firstOfThree (a :*: b :*: c) = a
```

Since inductive tuples are inductively defined, you may also choose to pattern
match on just the first few elements:

```haskell
firstOfN :: (a :*: rest) -> a
firstOfN (a :*: _) = a
```

Throughout the rest of this tutorial, we will simply use inductive tuples as if
they were "normal" tuples.


Creating and deleting databases
-------------------------------

You can use a table definition to create the corresponding table in your
database backend, as well as delete it.

```haskell
setup :: SeldaM ()
setup = do
  createTable people
  createTable addresses

teardown :: SeldaM ()
teardown = do
  tryDropTable people
  tryDropTable addresses
```

Both creating and deleting tables comes in two variants: the `try` version
which is a silent no-op when attempting to create a table that already exists
or delete one that doesn't, and the "plain" version which raises an error.


Inserting data
--------------

Data insertion is done in batches. To insert a batch of rows, pass a list of
rows where each row is an inductive tuple matching the type of the table.
Optional values are encoded as `Maybe` values.

```haskell
populate :: SeldaM ()
populate = do
  insert_ people
    [ "Link"      :*: 125 :*: Just "horse"
    , "Velvet"    :*: 19  :*: Nothing
    , "Kobayashi" :*: 23  :*: Just "dragon"
    , "Miyu"      :*: 10  :*: Nothing
    ]
  insert_ addresses
    [ "Link"      :*: "Kakariko"
    , "Kobayashi" :*: "Tokyo"
    , "Miyu"      :*: "Fuyukishi"
    ]
```

Insertions come in two variants: the "plain" version which reports back the
number of inserted rows, and one appended with an underscore which returns `()`.
Use the latter to explicitly indicate your intent to ignore the return value.

The following example inserts a few rows into a table with an
auto-incrementing primary key:

```haskell
people' :: Table (RowID :*: Text :*: Int :*: Maybe Text)
people' = table "people_with_ids"
  $   autoPrimary "id"
  :*: required "name"
  :*: required "age"
  :*: optional "pet"

populate' :: SeldaM ()
populate' = do
  insert_ people'
    [ def :*: "Link"      :*: 125 :*: Just "horse"
    , def :*: "Velvet"    :*: 19  :*: Nothing
    , def :*: "Kobayashi" :*: 23  :*: Just "dragon"
    , def :*: "Miyu"      :*: 10  :*: Nothing
    ]
```

Note the use of the `def` value for the `id` field. This indicates that the
default value for the column should be used in lieu of any user-provided value.
Since the `id` field is an auto-incrementing primary key, it will automatically
be assigned a unique, increasing value.
Thus, the resulting table would look like this:

```
id | name      | age | pet
-----------------------------
 0 | Link      | 125 | horse
 1 | Velvet    | 19  |
 2 | Kobayashi | 23  | dragon
 3 | Miyu      | 10  |
```

Auto-incrementing primary keys must always have the type `RowID`.


Updating rows
-------------

To update a table, pass the table and two functions to the `update` function.
The first is a predicate over table columns. The second is a mapping over table 
columns, specifying how to update each row. Only rows satisfying the predicate 
are updated.

```haskell
age10Years :: SeldaM ()
age10Years = do
  update_ people (\(name :*: _ :*: _) -> name ./= "Link")
                 (\(name :*: age :*: pet) -> name :*: age + 10 :*: pet)
```

Note that you can use arithmetic, logic and other standard SQL operations on
the columns in either function. Columns implement the appropriate numeric
type classes. For operations with less malleable types -- logic and
comparisons, for instance -- the standard Haskell operators are prefixed
with a period (`.`).


Deleting rows
-------------

Deleting rows is quite similar to updating them. The only difference is that
the `deleteFrom` operation takes a table and a predicate, specifying which rows
to delete.
The following example deletes all minors from the `people` table:

```haskell
byeMinors :: SeldaM ()
byeMinors = deleteFrom_ people (\(_ :*: age :*: _) -> age .< 20)
```


Basic queries
-------------

Queries are written in the `Query` monad, in which you can query tables,
restrict the result set, and perform inner, aggregate queries.
Queries are executed in some Selda monad using the `query` function.

The following example uses the `select` operation to draw each row from the
`people` table, and the `restrict` operation to remove out all rows except
those having an `age` column with a value greater than 20.


```haskell
grownups :: Query s (Col s Text)
grownups = do
  (name :*: age :*: _) <- select people
  restrict (age .> 20)
  return name

printGrownups :: SeldaM ()
printGrownups = do
  names <- query grownups
  liftIO (print names)
```

You may have noticed that in addition to the return type of a query,
the `Query` type has an additional type parameter `s`.
We'll cover this parameter in more detail when we get to
aggregating queries, so for now you can just ignore it.


Selector functions
------------------

It's often annoying to explicitly take the tuples returned by queries apart.
For this reason, Selda provides a function `selectors` to generate
*selectors*: identifiers which can be used with the `!` operator to access
elements of inductive tuples similar to how record selectors are used to access
fields of standard Haskell record types.

Rewriting the previous example using selector functions:

```haskell
name :*: age :*: pet = selectors people

grownups :: Query s (Col s Text)
grownups = do
  p <- select people
  restrict (p ! age .> 20)
  return (p ! name)

printGrownups :: SeldaM ()
printGrownups = do
  names <- query grownups
  liftIO (print names)
```

For added convenience, the `tableWithSelectors` function creates both a table
and its selector functions at the same time:

```haskell
posts :: Table (RowID :*: Maybe Text :*: Text)
(posts, postId :*: author :*: content)
  =   tableWithSelectors "posts"
  $   autoPrimary "id"
  :*: optional "author"
  :*: required "content"

allAuthors :: Query s Text
allAuthors = do
  p <- select posts
  return (p ! author)
```

You can also use selectors with the `with` function to update columns in a tuple.
`with` takes a tuple and a list of *assignments*, where each assignment is a
selector-value pair. For each assignment, the column indicated by the selector
will be set to the corresponding value, on the given tuple.

```haskell
grownupsIn10Years :: Query s (Col s Text)
grownupsIn10Years = do
  p <- select people
  let p' = p `with` [age := p ! age + 10]
  restrict (p' ! age .> 20)
  return (p' ! name)
```

Of course, selectors can be used for updates and deletions as well.

For the remainder of this tutorial, we'll keep matching on the tuples
explicitly.


Products and joins
------------------

Of course, data can be drawn from multiple tables. The unfiltered result set
is essentially the cartesian product of all queried tables.
For this reason, `restrict` calls should be made as early as possible, to avoid
creating an unnecessarily large result set.

Arbitrary Haskell values can be injected into queries. As injected values are
passed as parameters to prepared statements under the hood, there is no need
to escape data; SQL injection is impossible by construction.

The following example uses data from two tables to find all grown-ups who
reside in Tokyo. Note the use of the `text` function, to convert a Haskell
`Text` value into an SQL column literal, as well as the use of `name .== name'`
to remove all elements from the result set where the name in the `people` table
does not match the one in the `addresses` table.

```haskell
grownupsIn :: Text -> Query s (Col s Text)
grownupsIn city = do
  (name :*: age :*: _) <- select people
  restrict (age .> 20)
  (name' :*: home) <- select addresses
  restrict (home .== text city .&& name .== name')
  return name

printGrownupsInTokyo :: SeldaM ()
printGrownupsInTokyo = do
  names <- query (grownupsIn "Tokyo")
  liftIO (print names)
```

Also note that this is slightly different from an SQL join. If, for instance,
you wanted to get a list of all people and their addresses, you might do
something like this:

```haskell
allPeople :: Query s (Col s Text :*: Col s Text)
allPeople = do
  (people_name :*: _ :*: _) <- select people
  (addresses_name :*: city) <- select addresses
  restrict (people_name .== addresses_name)
  return (people_name :*: city)
```

This will give you the list of everyone who has an address, resulting in the
following result set:

```
name      | city
---------------------
Link      | Kakariko
Kobayashi | Tokyo
Miyu      | Fuyukishi
```

Note the absence of Velvet in this result set. Since there is no entry for
Velvet in the `addresses` table, there can be no entry in the product table
`people × addresses` where both `people_name` and `addresses_name` are equal
to `"Velvet"`. To produce a table like the above but with a `NULL` column for
Velvet's address (or for anyone else who does not have an entry in the
`addresses` table), you would have to use a join:

```haskell
allPeople' :: Query s (Col s Text :*: Col s (Maybe Text))
allPeople' = do
  (name :*: _ :*: _) <- select people
  (_ :*: city) <- leftJoin (\(name' :*: _) -> name .== name')
                           (select addresses)
  return (name :*: city)
```

This gives us the result table we want:

```
name      | city
---------------------
Link      | Kakariko
Velvet    |
Kobayashi | Tokyo
Miyu      | Fuyukishi

```

The `leftJoin` function left joins its query argument to the current result set
for all rows matching its predicate argument.
Note that all columns returned from the inner (or right) query are converted by
`leftJoin` into nullable columns. As there may not be a right counter part for
every element in the result set, SQL and Selda alike set any missing joined
columns to `NULL`.


Aggregate queries, grouping and sorting
---------------------------------------

You can also perform queries that sum, count, or otherwise aggregate their
result sets. This is done using the `aggregate` function.
This is where the additional type parameter to `Query` comes into play.
When used as an inner query, aggregate queries must not depend on any columns
from the outer query. To enforce this, the `aggregate` function forces all
operations to take place in the `Query (Inner s)` monad, if the outer query
takes place in the `Query s` monad. This ensures that aggregate inner queries
can only communicate with their outside query by returning some value.

Like in standard SQL, aggregate queries can be grouped by column name or by
some arbitrary expression.
An aggregate subquery must return at least one aggregate column, obtained using
`sum_`, `avg`, `count`, or one of the other provided aggregate functions.
Note that aggregate columns, having type `Aggr s a`, are different from normal
columns of type `Col s a`.
Since SQL does not allow aggregate functions in `WHERE` clauses, Selda prevents
them from being used in arguments to `restrict`.

The following example uses an aggregate query to calculate how many home each
person has, and order the result set with the most affluent homeowners at the
top.

```haskell
countHomes :: Query s (Col s Text :*: Col s Int)
countHomes = do
  (name :*: _ :*: _) <- select people
  (owner :*: homes) <- aggregate $ do
    (owner :*: city) <- select addresses
    owner' <- groupBy owner
    return (owner' :*: count city)
  restrict (owner .== name)
  order homes descending
  return (owner :*: homes)
```

Note how `groupBy` returns an aggregate version of its argument, which can be
returned from the aggregate query. In this example, returning `owner` instead of
`owner'` wouldn't work since the former is a plain column and not an aggregate.


Transactions
------------

All databases supported by Selda guarantee that each query is atomic: either
the entire query is performed in one go, with no observable intermediate state,
or the whole query fails without leaving a trace in the database.
However, sometimes this guarantee is not enough.
Consider, for instance, a money transfer from Alice's bank account to Bob's.
This involves at least two queries: one to remove the money from
Alice's account, and one to add the same amount to Bob's.
Clearly, it would be *bad* if this operation were to be interrupted after
withdrawing the money from Alice's account but before depositing it into Bob's.

The solution to this problem is *transactions*: a mechanism by which
*a list of queries* gain the same atomicity guarantees as a single query always
enjoys. Using transactions in Selda is super easy:

```haskell
transferMoney :: Text -> Text -> Double -> SeldaM ()
transferMoney from to amount = do
  transaction $ do
    update_ accounts (\(owner :*: _) -> owner .== text from)
                     (\(owner :*: money) -> owner :*: money - float amount)
    update_ accounts (\(owner :*: _) -> owner .== text to)
                     (\(owner :*: money) -> owner :*: money + float amount)
```

This is all there is to it: pass the entire computation to the `transaction`
function, and the whole computation is guaranteed to either execute atomically,
or to fail without leaving a trace in the database.
If an exception is raised during the computation, it will of course be rolled
back.

Do be careful, however, to avoid performing IO within a query.
While they will not affect the atomicity of the computation as far as the
database is concerned, the computations themselves can obviously not be
rolled back.


In-process caching
------------------

In many applications, read operations are orders of magnitude more common than
write operations. For such applications, it is often useful to *cache* the
results of a query, to avoid having the database perform the same, potentially
heavy, query over and over even though we *know* we'll get the same result
every time.

Selda supports automatic caching of query results out of the box.
However, it is turned off by default.
To enable caching, use the `setLocalCache` function.

```haskell
main = withPostgreSQL connection_info $ do
  setLocalCache 1000
  ...
```

This will enable local caching of up to 1,000 different results.
When that limit is reached, the least recently used result will be discarded,
so the next request for that result will need to actually execute the query
on the database backend.
If caching was already enabled, changing the maximum number of cached results
will discard the cache's previous contents.
Setting the cache limit to 0 disables caching again.

To make sure that the cache is always consistent with the underlying database,
Selda keeps track of which tables each query depends on.
Whenever an insert, update, delete or drop is issued on a table `t`, all cached
queries that depend on `t` will be discarded.

This guarantees consistency between cache and database, but *only* under the
assumption that *no other process will modify the database*.
If this assumption does not hold for your application, you should avoid using
in-process caching.
It is perfectly fine, however, to have multiple *threads* within the same
application modifying the same database as long as they're all using Selda
to do it, as the cache is shared between all Selda computations
running in the same process.


Generic tables and queries
--------------------------

Selda also supports building tables and queries from (almost) arbitrary
data types, using the `Database.Selda.Generic` module.
Re-implementing the ad hoc `people` and `addresses` tables from before in a
more disciplined manner in this way is quite easy:

```haskell
data Person = Person
  { personName :: Text
  , age        :: Int
  , pet        :: Maybe Int
  } deriving Generic

data Address = Address
  { addrName :: Text
  , city     :: Text
  } deriving Generic


people :: GenTable Person
people = genTable "people" [personName :- primaryGen]

addresses :: GenTable Address
addresses = genTable "addresses" [personName :- primaryGen]
```

This will declare two tables with the same structure as their ad hoc
predecessors. Creating the tables is similarly easy:

```haskell
create :: SeldaM ()
create = do
  createTable (gen people)
  createTable (gen addresses)
```

Note the use of the `gen` function here, to extract the underlying table of
columns from the generic table.

However, queries over generic tables aren't magic; they still consist of the
same collections of columns as queries over non-generic tables.

```haskell
genericGrownups2 :: Query s (Col s Text)
genericGrownups2 = do
  (name :*: age :*: _) <- select (gen people)
  restrict (age .> 20)
  return name
```

Of course, we can also use selectors together with generics:

```
sel_name :*: sel_age :*: sel_pet = selectors (gen people)
```

With generics it's also quite easy to re-assemble Haskell objects
from the results of a query using the `fromRel` function, or its handy relative
`fromRels`.

```haskell
getPeopleOfAge :: Int -> SeldaM [Person]
getPeopleOfAge yrs = do
  ps <- query $ do
    (name :*: age :*: _) <- select (gen people)
    restrict (age .== yrs)
    return p
  return (fromRels ps) -- equivalent to return (map fromRel ps)
```

Sometimes, we want to relate two entire relations to each other rather than
just two columns.
In Selda, we can do this by concatenating two (or more) relations and returning
the result. When working with generics, we can also automatically convert the
returned relations into inductive tuples of their corresponding data types.

The following query joins the `people` table with itself to produce the list of
pairs of people where the first person is older than the second.

```haskell
getOlderThanPairs :: SeldaM [Person :*: Person]
getOlderThanPairs = do
  ps <- query $ do
    person1 <- select (gen people)
    person2 <- select (gen people)
    restrict (person1!sel_name .> person2!sel_name)
    return (person1 .++ person2)
  return (fromRels ps)
```


Prepared statements
-------------------

While Selda makes use of prepared statements internally to ensure that any and
all input is safely escaped, it does not reuse those statements by default.
Every query is recompiler and replanned each time it is executed.
To improve the performance of your code, you should make use of the `prepared`
function, to mark performance-critical queries as reusable.

The `prepared` function converts any function `f` in the `Query` monad into an
equivalent function `f'` in some `MonadSelda`, provided that all of `f`'s
arguments are column expressions.
When `f'` is called for the first time during a connection to a database, it
automatically gets compiled, prepared and cached before being executed.
Any subsequent calls to `f'` from the same connection will reuse the prepared
version.

Note that since most database engines don't allow prepared statements to persist
across connections, a previously cached statement will get prepared once more if
called from another connection.

As an example, we modify the `grownupsIn` function we saw earlier to use prepared
statements.

```haskell
preparedGrownupsIn :: Text -> SeldaM [Text]
preparedGrownupsIn = prepared $ \city -> do
  (name :*: age :*: _) <- select people
  restrict (age .> 20)
  (name' :*: home) <- select addresses
  restrict (home .== city .&& name .== name')
  return name
```

Note that the type of the `city` argument is `Col s Text` within the query, but
when *calling* `preparedGrownupsIn`, we instead pass in a value of type `Text`;
for convenience, `prepared` automatically converts all arguments to
prepared functions into their equivalent column types.


Foreign keys
------------

To add a foreign key constraint on a column, use the `fk` function.
This function takes two parameters: a column of the table being defined, and
a tuple of the `(table, column)` the foreign key refers to.
The table identifier is simply a value of type `Table t`, while the column
is specified using a selector of type `Selector t a`.

The following example creates a table to store users, and one to store blog
posts. The `users` table stores a name, a password, and a unique identifier
for each user.
The `posts` table stores, for each post, the post body, a unique
post identifier, and the identifier of the user who wrote the post.
The column storing a post's author has a foreign key constraint on the `userid`
column of the `users` table, to ensure that each post has a valid author.

```haskell
users :: Table (RowID :*: Text)
users = table "users"
  $   primary "userid"
  :*: required "username"
  :*: required "password"
(userId :*: userName :*: userPass) = selectors users

posts :: Table (RowID :*: RowID :*: Text)
posts = table "posts"
  $   primary "postid"
  :*: required "authorid" `fk` (users, userId)
  :*: required "post_body"
```

Note that a foreign key can *only* refer to a column which is either
a primary key or has a unique constraint. This is not specific to Selda, but
a restriction of SQL.

And with that, we conclude this tutorial. Hopefully it has been enough to get
you comfortably started using Selda.
For a more detailed API reference, please see Selda's
[Haddock documentation](http://hackage.haskell.org/package/selda).


Hacking
=======

Contributing
------------

All forms of contributions are welcome!

If you have a bug to report, please try to include as much information as
possible, preferably including:

* A brief description (one or two sentences) of the bug.
* The version of Selda+backend where the bug was found.
* A step-by-step guide to reproduce the bug.
* The *expected* result from following these steps.
* What *actually* happens when following the steps.
* Which component contains the bug (selda, selda-sqlite or selda-postgresql),
  if you're reasonably sure about where the bug is.

Bonus points for a small code example that illustrates the problem.

If you want to contribute code, please consult the following checklist before
sending a pull request:

* Does the code build with a recent version of GHC?
* Do all the tests pass?
* Have you added any tests covering your code?


Setting up the build environment
--------------------------------

From the repository root:

* Install `libpq-dev` from your package manager.
    This is required to build the PostgreSQL backend.
* Run `make sandbox` followed by `make deps`.
    This will set up a local sandbox for your Selda hacking, and install all
    Haskell necessary dependencies.
* Familiarise yourself with the various targets in the makefile.
    The dependencies between Selda, the backends and the tests are slightly
    complex, so straight-up cabal is too quirky for day to day hacking.


Setting up a VM for PostgreSQL testing
--------------------------------------

While the SQLite backend is completely self-contained, the PostgreSQL backend
needs an appropriate server for testing. Setting this up in a virtual machine
is definitely less intrusive than setting up a server on your development
machine. To set up a VM for the PostgreSQL backend tests:

* Install your favourite hypervisor, such as VMWare player or VirtualBox.
* Download a pre-built PostgreSQL VM from
    [Bitnami](https://bitnami.com/stack/postgresql/virtual-machine).
* Import the OVA file into your hypervisor.
* Change the network settings of your newly imported VM to NAT, and make sure
    that port 5432 is forwarded. Note that this will conflict with any PostgreSQL
    server running on your machine while the VM is running.
* Boot your VM and note the password displayed on the login screen.
* Create the file `selda-tests/PGConnectInfo.hs` with the following content:
    ```haskell
    {-# LANGUAGE OverloadedStrings #-}
    module PGConnectInfo where
    import Database.Selda.PostgreSQL
    
    pgConnectInfo = "test" `on` "localhost" `auth` ("postgres", "$PASSWORD")
    ```
    Where `$PASSWORD` is the password from the VM's login screen.
* Log in to the VM and disable the built-in firewall by running
    `sudo systemctl disable ufw ; sudo systemctl stop ufw`.
* From your host machine, create the test database:
    ```
    $ psql -h 127.0.0.1 -U postgres -W
    [password from login screen]
    # CREATE TABLE test;
    # \q
    ```
* Run `make pgtest` to check that everything works.


TODOs
-----

Features that would be nice to have but are not yet implemented.

* Monadic if/else.
* Streaming
* Type-safe migrations
* `SELECT INTO`.
* Database schema upgrades.
* Stack build.
* MySQL/MariaDB backend.
* Automatically sanity check changelog, versions and date before release.
