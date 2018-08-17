<div class="pane" id="left">

## Chapter 2: Destructive Operations

A database we can't modify in any meaningful way is no good, so let's
have a closer look at how to make our database reflect changes to the real world.
Throughout this chapter, we're going to work with the table and data presented
to the right of the text.

After reading this chapter, you will be able to add, update and delete
table rows. You will also gain a better understanding of the types involved,
as all examples from this chapter on include full type signatures.

### All About Inserts

As we saw in the previous chapter, inserting data into a table is easy.
However, the table from the first chapter is often a bit too simplistic.
Recall that we used the name of a person as the table's primary key.
What if the user changes their name, or if there are multiple persons with
the same name?

For reasons like this, we often want the database to keep track of primary
keys for us, so for this chapter we've added the `pid` column to the table,
which will at all times contain the unique identifier of each row.

```language-haskell
data Person = Person
  { pid  :: ID Person
  ...
```

Since we want the database to manage the identifier for us, we have to add a
column attribute, specifying that `pid` is an *auto-incrementing* primary key.

```language-haskell
people :: Table Person
people = table "people" [field @"pid" :- autoPrimary]
```

With these simple changes, we can now use the somewhat magical `def` value when
inserting data, to tell the database that we want to use the *default* value
for the `pid` column.

It is worth noting that `def` can be used for *any* column to which we want to
assign a default value. However, it is only when used as an auto-incrementing
primary key that `def` ensures the uniqueness of the value; in all other cases
it is simply the "least" value of the type, such as `False`, `""` or `Nothing`.

```language-haskell
insertSara :: SeldaM ()
insertSara = insert_ people [Person def "Sara" 14 Nothing]
```

If we perform the insert and then select all table rows, we'll find that Sara
was added to the table with a unique identifier.

```language-haskell
insertThenInspect :: SeldaM [Person]
insertThenInspect = do
  insertSara
  query (select people)
```

```language-haskell
pid | name      | age | pet
---------------------------
  1 | Velvet    | 19  | Just Dog
  2 | Kobayashi | 23  | Just Dragon
  3 | Miyu      | 10  | Nothing
  4 | Sara      | 14  | Nothing
```

When using auto-incrementing primary keys, we often want to know what ID
a particular row got when we inserted it.
The `insertWithPK` function is handy for this.

```language-haskell
insertAndPrintPk :: SeldaM ()
insertAndPrintPk = do
  pk <- insertWithPK people [Person def Sara 14 Nothing]
  liftIO (print pk)
```
```language-haskell
> withSQLite "database.sqlite" insertAndPrintPk
4
```

### Updates

As time goes by, people age, their pets die, and thus our `people` table
will need to be regulary updated to stay in sync with reality.
Fortunately, Selda's got your back:

```language-haskell
update :: (MonadSelda m, Relational a)
       => Table a
       -> (Row s a -> Col s Bool)
       -> (Row s a -> Row s a)
       -> m Int
```

The `update` function takes a table, a predicate, and an update function as its
inputs, and returns the number of rows affected by the update.
For each row that matches the given predicate &mdash; where the predicate
returns `true` &mdash Selda applies the given update function.

Let's assume, for instance, that we're feeling sorry for anyone who hasn't got
a pet, and decide to give every pet-less person a dog:

```language-haskell
petsForEveryone :: SeldaM Int
petsForEveryone = do
  update people
         (\person -> isNull (person ! field @"pet"))
         (\person -> person `with` [field @"pet" := just (literal Dog)])
```

Here we see the use of `with` for the first time.
The `with` function takes a table row and a list of updates &mdash; expressed
as selector-expression pairs &mdash; and applies the updates,
from left to right, to the table row.

The most basic update is `:=`, which simply overwrites the column
indicated by its given selector with its given expression, but there are also
convenient shortcuts for common operations such as incrementing or decrementing
values:

```language-haskell
ageEveryone :: SeldaM Int
ageEveryone = do
  update people (const true) (\person -> person `with` [field @"age" += 1])
```

As expected, this function will increment everyone's age by one.

### Conditional Updates

Sometimes it can be useful to update a table row *or* add it to the table if
it doesn't exist. This is sometimes referred to as an *upsert*.
In Selda, this is handled by the `upsert` function.

```language-haskell
upsert :: ( MonadSelda m
          , Relational a
          )
       => Table a
       -> (Row s a -> Col s Bool)
       -> (Row s a -> Row s a)
       -> [a]
       -> m (Maybe (ID a))
```

This function works much like `update`, but takes an additional list of elements,
to be inserted into the table iff *no rows currently in the given table match
the given predicate*.

To grant a horse to any person named Miyu, and to create a horse owner by that
name if none exists, we would do the following:

```language-haskell
horseForMiyu :: SeldaM ()
horseForMiyu = do
  result <- upsert people
                   (\person -> person ! field @"name" .== "Miyu")
                   (\person -> person `with` [field @"pet" := just (literal Horse)])
                   [Person def Miyu 10 Nothing]
  case result of
    Just id -> liftIO $ putStrLn ("person inserted with id " ++ show id)
    Nothing -> liftIO $ putStrLn "update performed; no person inserted"
```
```language-haskell
> withSQLite "database.sqlite" horseForMiyu
update performed; no person inserted
```

Apart from `upsert`, there are several less general conditional inserts, which
often result in clearer code.
Checking out the [API documentation](https://hackage.haskell.org/package/selda)
is highly recommended.

### Deleting Rows

Compared to inserts and updates, there is really not much to say about delete
operations.

```language-haskell
deleteFrom :: (MonadSelda m, Relational a)
           => Table a
           -> (Row s a -> Col s Bool)
           -> m Int
```

The delete operation takes a table to delete stuff from, and a predicate
determining which rows to delete.
Every row that matches the predicate is deleted, and `deleteFrom` returns
the number of deleted rows.

Let's say, for instance, that we want to delete all dragon owners from the
table (they've probably gotten eaten already anyway):

```language-haskell
deleteDragonOwners :: SeldaM Int
deleteDragonOwners = do
  deleteFrom persons (\person -> person ! field @"pet" .== just (literal Dragon))
```
```language-haskell
> withSQLite "database.sqlite" deleteDragonOwners
1
> withSQLite "database.sqlite" (query $ select persons)
pid | name   | age | pet
------------------------
  1 | Velvet | 19  | Just Dog
  3 | Miyu   | 10  | Nothing
```

</div>

<div class="pane fixed" id="right">

### The Table

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

people :: Table Person
people = table "people" [field @"id" :- autoPrimary]
```

### The Data

```language-haskell
pid | name      | age | pet
---------------------------
  1 | Velvet    | 19  | Just Dog
  2 | Kobayashi | 23  | Just Dragon
  3 | Miyu      | 10  | Nothing
```

**Next:** [Chapter 3: Advanced Queries](tutorial/ch3-advanced-queries)<br>
**Previous:** [Chapter 1: An Example, Explained](tutorial/ch1-example-explained)

</div>
