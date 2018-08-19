<div class="pane" id="left">

## Chapter 3: Advanced Queries

While fetching rows from a single table is all well and good, most applications
require a bit more interesting queries.
This chapter will teach you how to correlate data between multiple tables, and
to conjure up table rows from thin air.


### Product Queries

Perhaps the most basic capability of a relational database is to correlate
data between different tables, hence the *relational* part.
In Selda, the monadic bind operator performs this function.
In less theoretical terms, to combine data from multiple tables, simply use
the `select` operation on each desired table.
This mechanism should be familiar to you if you've ever used list comprehensions.

To illustrate the concept, this query returns a list of pairs of all people
in our database and any homes they happen to own.

```language-haskell
peopleWithHomes :: Query s (Row s Person :*: Row s Home)
peopleWithHomes = do
  person <- select people
  home <- select homes
  restrict (person ! #name .== home ! #ownerName)
  return (person :*: home)
```

The type signature of this query merits some explanation, as this is the first
time we've seen one explicitly spelled out.
As mentioned in [chapter 1](tutorial/ch1-example-explained), all Selda queries
are executed in the `Query` monad, which is parameterised over
a *scope type* `s`.

All rows and columns originating from any given scope `s` are tagged with that
`s`. This prevents queries in *another* scope from accessing those values, as
all Selda operations ensure that the `s` of the current query and the `s` of the
data being operated on are identical.

So far, we haven't encountered any queries that are convoluted enough for this
to matter. Once we get to
[joins and aggregates](tutorial/ch4-joins-and-aggregates) however, the necessity
of the scope parameter will become apparent.


### Set Membership

The `peopleWithHomes` query does not include any persons who *don't* have
a home. Not wanting to marginalize homeless people further, let's write a
function that finds all such persons.

The set of homeless people can be defined as the set of people who do
*not* appear in the set of home owners.
This means that we can check whether any particular person is homeless
using the `isIn` function, which determines whether some
particular value is contained in some other result set.

```language-haskell
homelessPeople :: Query s (Row s Person)
homelessPeople = do
  person <- select people
  restrict (not_ $ (person ! #name) `isIn` (#ownerName `from` select homes))
  return person
```

The set of home owners is produced by the query
``#ownerName `from` select homes``.
The `from` function is a convenient shorthand for extracting a single column
from a query, and is defined as `from s q = fmap (!s) q`.


### Ad Hoc Rows

Sometimes, it can be handy to conjure up database rows "from thin air".
Let's say, for instance, that we want to create a `Person` row with only
the information present in the `homes` table.
We can accomplish this by using the `new` function.

```language-haskell
personsFromHomes :: Query s (Row s Person)
personsFromHomes = do
  home <- select homes
  return $ new [#name := home ! #ownerName]
```

`new` takes a list of updates &mdash; as seen
in [chapter 2](tutorial/ch2-destructive-operations) &mdash; which are applied
to a row where each value initially has its type's default value.
Thus, the `personsFromHomes` function will return a list of `Person` values
with their names set to something sensible, and the other fields set to
`0` or `Nothing`.

### Querying into Another Table

This ability to create entirely new rows can be convenient when you want to
return some particular type from a query, but you don't want to get it straight
from a table.
One prime example of this is the `queryInto` function &mdash; corresponding to
the `SELECT INTO` SQL statement &mdash; which inserts the result set of a query
straight into another table and returns the number of inserted rows.

Using this function, we can remedy the homelessness situation by simply granting
a cheap home in Tokyo to any homeless persons in the `people` table.

```language-haskell
homesForEveryone :: SeldaM Int
homesForEveryone = queryInto homes $ do
  person <- select people
  restrict (not_ $ (person ! #name) `isIn` (#ownerName `from` select homes))

  return $ new
    [ #ownerName := person ! #names
    , #city := "Tokyo"
    , #rent := 50000
    ]
```

Using the `TypeApplications` language extension, we can make the `new` expression
even clearer, explicitly telling the compiler just what type of row
we want to conjure up:

```language-haskell
return $ new @Home [ ... ]
```

This is often useful &mdash; essential, even &mdash; when, for instance,
creating an row entirely made up of default values, with no updates given
to fix the type of the row.

</div>

<div class="pane fixed" id="right">

### The Tables

```language-haskell
data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { pid  :: ID Person
  , name :: Text
  , age  :: Int
  , pet  :: Maybe Pet
  } deriving (Generic, Show)
instance SqlRow Person

data Home = Home
  { ownerName :: Text
  , city      :: Text
  , rent      :: Int
  } deriving (Generic, Show)
instance SqlRow Home

people :: Table Person
people = table "people" [#pid :- autoPrimary]

homes :: Table Home
homes = table "homes" []
```

<!-- **Next:** [Chapter 4: Joins and Aggregates](tutorial/ch4-joins-and-aggregates)<br> --!>

**Previous:** [Chapter 2: Destructive Operations](tutorial/ch2-destructive-operations)
</div>
