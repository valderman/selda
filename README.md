What is Selda?
==============

Selda is an SQL EDSL that was inspired by LINQ and
[Opaleye](http://hackage.haskell.org/package/opaleye).
After defining a database schema, Selda supports creating, deleting, updating
and querying tables.
All operations are type-safe, and should be reasonably performant.

Presently, SQLite is the only supported backend.


A brief tutorial
================

Defining a schema
-----------------

To work productively with Selda, you will need to enable the `TypeOperators` and
`OverloadedStrings` extensions.

Tables are defined as the product of one or more columns, expressed using
*extensible tuples*, or heterogeneous lists.
An extensible tuple is simply a list of values joined together by
the `:*:` operator.

```
people :: Table (Text :*: Int :*: Maybe Text)
people = table "people" $ primary "name" ¤ required "age" ¤ optional "pet"

addresses :: Table (Text :*: Text)
addresses = table "addresses" $ required "name" ¤ required "city"
```

Columns may be either `required` or `optional`.
Although the SQL standard supports nullable primary keys, Selda primary keys
are always required.


Running queries
---------------

Selda queries are run in the `SeldaT` monad transformer. Any `MonadIO` can be
extended with database capabilities. Throughout this tutorial, we will simply
use `SeldaT` on top of the plain `IO` monad.
`SeldaT` is entered using a backend-specific `withX` function. For instance,
the SQLite backend uses the `withSQLite` function:

```
main :: IO ()
main = withSQLite "my_database.sqlite" $ do
  people <- getAllPeople
  liftIO (print people)

getAllPeople :: SeldaT IO [Text :*: Int :*: Maybe Text]
getAllPeople = query (select people)
```

This will open the `my_database.sqlite` database for the duration of the
computation. If the computation terminates normally, or if it raises an
exception, the database is automatically closed.


Creating and deleting databases
-------------------------------

You can use a table definition to create the corresponding table in your
database backend, as well as delete it.

```
setup :: SeldaT IO ()
setup = do
  createTable people
  createTable addresses

teardown :: SeldaT IO ()
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
rows where each row is an extensible tuple matching the type of the table.
Optional values are encoded as `Maybe` values.

```
populate :: SeldaT IO ()
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


Updating rows
-------------

To update a table, pass the table and two functions to the `update` function.
The first is a mapping over table columns, specifying how to update each row.
The second is a predicate over table columns.
Only rows satisfying the predicate are updated.

```
age10Years :: SeldaT IO ()
age10Years = do
  update_ people (\(name :*: age :*: pet) -> name :*: age + 10 :*: pet)
                 (\(name :*: _ :*: _) -> name ./= "Link")
```

Note that you can use arithmetic, logic and other standard SQL operations on
the columns in either function. Columns implement the appropriate numeric
type classes. For operations with less malleable types -- logic and
comparisons, for instance -- the standard Haskell operators are prefixed
with a period (`.`).


Deleting rows
-------------

Deleting rows is quite similar to updating them. The only difference is that
the `delete` operation takes a table and a predicate, specifying which rows
to delete.
The following example deletes all minors from the `people` table:

```
byeMinors :: SeldaT IO ()
byeMinors = delete_ people (\(_ :*: age :*: _) -> age .< 20)
```


Basic queries
-------------

Queries are written in the `Query` monad, in which you can query tables,
restrict the result set, and perform inner, aggregate queries.
In addition to the return type of a query, the `Query` type has an additional
type parameter `s`. We'll cover this parameter in more detail when we get to
aggregation, so just ignore it for now.

The following example uses the `select` operation to draw each row from the
`people` table, and the `restrict` operation to remove out all rows except
those having an `age` column with a value greater than 20.


```
grownups :: Query s (Col s Text)
grownups = do
  name :*: age :*: _ <- select people
  restrict (age .> 20)
  return name

printGrownups :: SeldaT IO ()
printGrownups = do
  names <- query grownups
  liftIO (print names)
```


Products and joins
==================

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

```
grownupsIn :: Text -> Query s (Col s Text)
grownupsIn city = do
  name :*: age :*: _ <- select people
  restrict (age .> 20)
  name' :*: home <- select addresses
  restrict (home .== text city .&& name .== name')
  return name

printGrownupsInTokyo :: SeldaT IO ()
printGrownupsInTokyo = do
  names <- query (grownupsIn "Tokyo")
  liftIO (print names)
```

Also note that this is slightly different from an SQL join. If, for instance,
you wanted to get a list of all people and their addresses *if they have one*,
you might do something like this:

```
allPeople :: Query s (Col s Text :*: Col s Text)
allPeople = do
  people_name :*: _ :*: _ <- select people
  addresses_name :*: city <- select addresses
  restrict (people_name == addresses_name)
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
Velvet's address, you would have to use a join:

```
allPeople' :: Query s (Col s Text :*: Col s Maybe Text)
allPeople' = do
  name :*: _ :*: _ <- select people
  _ :*: city <- leftJoin (\(name' :*: _) -> name .== name')
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

```
countHomes :: Query s (Col s Text :*: Col s Int)
countHomes = do
  name :*: _ :*: _ <- select people
  owner :*: homes <- aggregate $ do
    owner :*: city <- select addresses
    groupBy owner
    return (count city :*: some owner)
  restrict (owner .== name)
  order homes descending
  return (owner :*: homes)
```


TODOs
=====

The following is a non-exhaustive list of tasks that need to be done before a
"proper" release.

* Insertions without NULL/autoincrement columns.
* Insertions returning the last inserted primary key.
* Foreign keys.
* Constraints other than primary key.
* If/else.
* PostgreSQL backend.
* Tests.
* Examples.
* Split backends into separate packages.
* Stack support.
