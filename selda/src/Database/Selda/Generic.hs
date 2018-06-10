{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GADTs, CPP, DeriveGeneric, DataKinds #-}
-- | Build tables and database operations from (almost) any Haskell type.
--
--   While the types in this module may look somewhat intimidating, the rules
--   for generic tables and queries are quite simple:
--
--     * Any record type with a single data constructor, where all fields are
--       instances of 'SqlType', can be used for generic tables and queries
--       if it derives 'Generic'.
--     * Record types fulfilling the above criteria may also be included within
--       other relations by wrapping it in the 'Nested' type.
--     * Columns from several tables can be concatenated and returned
--       from queries.
--       If these concatenated columns match up with a sequence of
--       record types, the columns can be marshalled into the appropriate
--       sequence of record types using inductive tuples, by calling i.e.
--       @let foo = fromRels result :: [Person :*: Person]@.
--     * To use the standard functions from "Database.Selda" on a generic table,
--       it needs to be unwrapped using 'gen'.
--     * Performing a 'select' on a generic table returns all the table's fields
--       as an inductive tuple.
--     * Tuples obtained this way can be handled either as any other tuple, or
--       using the '!' operator together with any record selector for the
--       tuple's corresponding type.
--     * Relations obtained from a query can be re-assembled into their
--       corresponding data type using 'fromRel'.
module Database.Selda.Generic
  ( Relational, Generic, FromRel, ToDyn
  , GenAttr (..), GenTable (..), Attribute, Relation, Relations, Nested (..)
  , genTable, genTableFieldMod, toRel, toRels, fromRel, fromRels
  , insertGen, insertGen_, insertGenWithPK
  , primaryGen, autoPrimaryGen, uniqueGen, fkGen
  ) where
import Control.Monad.State
import Data.Dynamic
import Data.Text (pack)
#if MIN_VERSION_base(4, 10, 0)
import Data.Typeable
#endif
import GHC.Generics hiding (R, (:*:), Selector)
import qualified GHC.Generics as G ((:*:)(..), Selector, R)
import Unsafe.Coerce
import Database.Selda hiding (from)
import Database.Selda.Table
import Database.Selda.Types
import Database.Selda.Selectors

-- | Any type which has a corresponding relation.
--   To make a @Relational@ instance for some type, simply derive 'Generic'.
--
--   Note that only types which have a single data constructor, and where all
--   fields are instances of 'SqlValue' can be used with this module.
--   Attempting to use functions in this module with any type which doesn't
--   obey those constraints will result in a very confusing type error.
type Relational a =
  ( Generic a
  , GRelation (Rep a)
  , GFromRel (Rep a)
  , ToDyn (Relation a)
  , Insert (Relation a)
  )

-- | Convert a generic type into the corresponding database relation.
--   A type's corresponding relation is simply the inductive tuple consisting
--   of all of the type's fields.
--
-- > data Person = Person
-- >   { id   :: Int
-- >   , name :: Text
-- >   , age  :: Int
-- >   , pet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > somePerson = Person 0 "Velvet" 19 Nothing
-- > (theId :*: theName :*: theAge :*: thePet) = toRel somePerson
--
--   This is mainly useful when inserting values into a table using 'insert'
--   and the other functions from "Database.Selda".
toRel :: Relational a => a -> Relation a
toRel = gToRel . from

-- | Convenient synonym for @map toRel@.
toRels :: Relational a => [a] -> [Relation a]
toRels = map toRel


-- | A generic table. Needs to be unpacked using @gen@ before use with
--   'select', 'insert', etc.
newtype GenTable a = GenTable {gen :: Table (Rel (Rep a))}

-- | The relation corresponding to the given Haskell type.
--   This relation simply corresponds to the fields in the data type, from
--   left to right. For instance:
--
-- > data Foo = Foo
-- >   { bar :: Int
-- >   , baz :: Text
-- >   }
--
--   In this example, @Relation Foo@ is @(Int :*: Text)@, as the first field
--   of @Foo@ has type @Int@, and the second has type @Text@.
type Relation a = Rel (Rep a)

-- | One or more 'Relation's concatenated.
--   Useful to return several relations from a query, and turn them back into
--   their Haskell equivalents using 'fromRel'.
type family Relations a where
  Relations (a :*: b) = Relations a :++: Relations b
  Relations a         = Relation a

-- | A generic column attribute.
--   Essentially a pair or a record selector over the type @a@ and a column
--   attribute.
data GenAttr a where
  (:-) :: (a -> b) -> Attribute -> GenAttr a

-- | Generate a table from the given table name and list of column attributes.
--   All @Maybe@ fields in the table's type will be represented by nullable
--   columns, and all non-@Maybe@ fields fill be represented by required
--   columns.
--   For example:
--
-- > data Person = Person
-- >   { id   :: Int
-- >   , name :: Text
-- >   , age  :: Int
-- >   , pet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > people :: GenTable Person
-- > people = genTable "people" [(name, autoPrimaryGen)]
--
--   This example will create a table with the column types
--   @Int :*: Text :*: Int :*: Maybe Text@, where the first field is
--   an auto-incrementing primary key.
genTable :: forall a. Relational a
         => TableName
         -> [GenAttr a]
         -> GenTable a
genTable tn attrs = genTableFieldMod tn attrs id

-- | Generate a table from the given table name,
--   a list of column attributes and a function
--   that maps from field names to column names.
--   Ex.:
--
-- > data Person = Person
-- >   { personId   :: Int
-- >   , personName :: Text
-- >   , personAge  :: Int
-- >   , personPet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > people :: GenTable Person
-- > people = genTableFieldMod "people" [(personName, autoPrimaryGen)] (stripPrefix "person")
--
--   This will create a table with the columns named
--   @Id@, @Name@, @Age@ and @Pet@.
genTableFieldMod :: forall a. Relational a
                 => TableName
                 -> [GenAttr a]
                 -> (String -> String)
                 -> GenTable a
genTableFieldMod tn attrs fieldMod = GenTable $ Table tn (validate tn (map tidy cols)) apk
  where
    dummy = mkDummy
    cols = zipWith addAttrs [0..] (tblCols (Proxy :: Proxy a) fieldMod)
    apk = or [AutoIncrement `elem` as | _ :- Attribute as <- attrs]
    addAttrs n ci = ci
      { colAttrs = colAttrs ci ++ concat
          [ as
          | f :- Attribute as <- attrs
          , identify dummy f == n
          ]
      , colFKs = colFKs ci ++
          [ thefk
          | f :- ForeignKey thefk <- attrs
          , identify dummy f == n
          ]
      }

-- | Re-assemble a generic type from its corresponding relation. This can be
--   done either for ad hoc queries or for queries over generic tables:
--
-- > data SimplePerson = SimplePerson
-- >   { name :: Text
-- >   , age  :: Int
-- >   }
-- >   deriving Generic
-- >
-- > demoPerson :: SimplePerson
-- > demoPerson = fromRel ("Miyu" :*: 10)
-- >
-- > adhoc :: Table (Text :*: Int)
-- > adhoc = table "adhoc" $ required "name" :*: required "age"
-- >
-- > getPersons1 :: MonadSelda m => m [SimplePerson]
-- > getPersons1 = map fromRel <$> query (select adhoc)
-- >
-- > generic :: GenTable SimplePerson
-- > generic = genTable "generic" []
-- >
-- > getPersons2 :: MonadSelda m => m [SimplePerson]
-- > getPersons2 = map fromRel <$> query (select (gen generic))
--
--   Applying @toRel@ to an inductive tuple which isn't the corresponding
--   relation of the return type is a type error.
fromRel :: (ToDyn (Relations a), FromRel a) => Relations a -> a
fromRel = fromRelInternal . toDyns

-- | Convenient synonym for @map fromRel@.
fromRels :: (ToDyn (Relations a), FromRel a) => [Relations a] -> [a]
fromRels = map fromRel

class Generic a => FromRel a where
  fromRelInternal :: [Dynamic] -> a

instance {-# OVERLAPPABLE #-}
  ( GFromRel (Rep a)
  , Generic a
  , ToDyn (Relations a)
  ) => FromRel a where
  fromRelInternal = to . fst . gFromRel

instance (GFromRel (Rep a), Generic a, FromRel a, FromRel b) =>
         FromRel (a :*: b) where
  fromRelInternal xs = to a' :*: fromRelInternal xs'
    where (a', xs') = gFromRel xs

-- | Like 'insertWithPK', but accepts a generic table and
--   its corresponding data type.
insertGenWithPK :: (Relational a, MonadSelda m) => GenTable a -> [a] -> m RowID
insertGenWithPK t = insertWithPK (gen t) . toRels

-- | Like 'insert', but accepts a generic table and its corresponding data type.
insertGen :: (Relational a, MonadSelda m) => GenTable a -> [a] -> m Int
insertGen t = insert (gen t) . toRels

-- | Like 'insert_', but accepts a generic table and its corresponding data type.
insertGen_ :: (Relational a, MonadSelda m) => GenTable a -> [a] -> m ()
insertGen_ t = void . insertGen t

-- | Some attribute that may be set on a table column.
data Attribute
  = Attribute [ColAttr]
  | ForeignKey (Table (), ColName)

-- | A primary key which does not auto-increment.
primaryGen :: Attribute
primaryGen = Attribute [Primary, Required, Unique]

-- | An auto-incrementing primary key.
autoPrimaryGen :: Attribute
autoPrimaryGen = Attribute [Primary, AutoIncrement, Required, Unique]

-- | A table-unique value.
uniqueGen :: Attribute
uniqueGen = Attribute [Unique]

-- | A foreign key constraint referencing the given table and column.
fkGen :: Table t -> Selector t a -> Attribute
fkGen (Table tn tcs tapk) (Selector i) =
  ForeignKey (Table tn tcs tapk, colName (tcs !! i))

-- | A dummy of some type. Encapsulated to avoid improper use, since all of
--   its fields are 'unsafeCoerce'd ints.
newtype Dummy a = Dummy a

-- | Extract all column names from the given type.
--   If the type is not a record, the columns will be named @col_1@,
--   @col_2@, etc.
tblCols :: forall a. (GRelation (Rep a)) => Proxy a -> (String -> String) -> [ColInfo]
tblCols _ fieldMod = zipWith pack' [0 :: Int ..] $ gTblCols (Proxy :: Proxy (Rep a)) fieldMod
  where
    pack' n ci = ci
      { colName = if colName ci == ""
                    then mkColName . pack $ "col_" ++ show n
                    else colName ci
      }

-- | Create a dummy of the given type.
mkDummy :: (Generic a, GRelation (Rep a)) => Dummy a
mkDummy = Dummy $ to $ evalState gMkDummy 0

-- | Get the selector identifier of the given selector for the given dummy.
identify :: Dummy a -> (a -> b) -> Int
identify (Dummy d) f = unsafeCoerce $ f d

-- | Allows nesting the given type within another type used as a relation.
--   For instance, the following code will produce a type error since relational
--   product types must normally contain only @SqlType@ types:
--
-- > data Foo = Foo Int Bool
-- > data Bar = Bar Text Foo
-- >
-- > tbl :: GenTable Bar
-- > tbl = genTable "some_table" []
--
--   However, by wrapping the @Foo@ in @Nested@, we tell Selda to flatten @Foo@
--   into @Bar@, resulting in a relation equivalent to
--   @Text :*: Int :*: Bool@:
--
-- > data Bar = Bar Text (Nested Foo)
--
--   Note that when generating selectors for a relation with flattened
--   components, the selector list is also flattened.
--   To generate selectors for the @Bar@ type:
--
-- > bar_text :*: bar_foo_int :*: bar_foo_bool = selectors (gen tbl)
newtype Nested a = Nested {unNest :: a}
  deriving (Show, Eq, Ord, Generic)

-- | The relation corresponding to the given type.
type family Rel (rep :: * -> *) :: * where
  Rel (M1 t c a)                     = Rel a
  Rel (K1 G.R (Nested a))            = Relation a
  Rel (K1 G.R a)                     = a
  Rel (a G.:*: b)                    = Rel a :++: Rel b

class GRelation f where
  -- | Convert a value from its Haskell type into the corresponding relation.
  gToRel   :: f a -> Rel f

  -- | Compute all columns needed to represent the given type.
  gTblCols :: Proxy f -> (String -> String) -> [ColInfo]

  -- | Create a dummy value where all fields are replaced by @unsafeCoerce@'d
  --   ints. See 'mkDummy' and 'identify' for more information.
  gMkDummy :: State Int (f a)

instance GRelation a => GRelation (M1 C c a) where
  gToRel (M1 x) = gToRel x
  gTblCols _ = gTblCols (Proxy :: Proxy a)
  gMkDummy = M1 <$> gMkDummy

instance GRelation a => GRelation (M1 D c a) where
  gToRel (M1 x) = gToRel x
  gTblCols _ = gTblCols (Proxy :: Proxy a)
  gMkDummy = M1 <$> gMkDummy

instance (G.Selector c, GRelation a) => GRelation (M1 S c a) where
  gToRel (M1 x) = gToRel x
  gTblCols _ fieldMod = colinfos'
    where
      colinfos = gTblCols (Proxy :: Proxy a) fieldMod
      colinfos' =
        case colinfos of
          [ci] -> [ColInfo
            { colName = mkColName . pack $ fieldMod (selName ((M1 undefined) :: M1 S c a b))
            , colType = colType ci
            , colAttrs = colAttrs ci
            , colFKs = colFKs ci
            }]
          _ -> colinfos
  gMkDummy = M1 <$> gMkDummy

instance (Rel (K1 i a) ~ a, Typeable a, SqlType a) => GRelation (K1 i a) where
  gToRel (K1 x) = x
  gTblCols _ _  = [ColInfo "" (sqlType (Proxy :: Proxy a)) optReq []]
    where
      -- workaround for GHC 8.2 not resolving overlapping instances properly
      maybeTyCon = typeRepTyCon (typeRep (Proxy :: Proxy (Maybe ())))
      optReq
        | typeRepTyCon (typeRep (Proxy :: Proxy a)) == maybeTyCon = [Optional]
        | otherwise                                               = [Required]
  gMkDummy = do
    n <- get
    put (n+1)
    return $ unsafeCoerce n

instance {-# OVERLAPS #-}
  ( Typeable a
  , GRelation (Rep a)
  , Rel (K1 i (Nested a)) ~ Relation a
  , Generic a
  ) => GRelation (K1 i (Nested a)) where
  gToRel (K1 (Nested x)) = gToRel (from x)
  gTblCols _ = gTblCols (Proxy :: Proxy (Rep a))
  gMkDummy = fmap (K1 . Nested . to) gMkDummy

instance (Append (Rel a) (Rel b), GRelation a, GRelation b) =>
         GRelation (a G.:*: b) where
  gToRel (a G.:*: b)   = gToRel a `app` gToRel b
  gTblCols _ f = gTblCols a f ++ gTblCols b f
    where
      a = Proxy :: Proxy a
      b = Proxy :: Proxy b
  gMkDummy = do
    a <- gMkDummy :: State Int (a x)
    b <- gMkDummy :: State Int (b x)
    return (a G.:*: b)

class GFromRel f where
  -- | Convert a value to a Haskell type from the type's corresponding relation.
  gFromRel :: [Dynamic] -> (f a, [Dynamic])

instance (GFromRel a, GFromRel b) => GFromRel (a G.:*: b) where
  gFromRel xs =
      (x G.:*: y, xs'')
    where
      (x, xs') = gFromRel xs
      (y, xs'') = gFromRel xs'

instance Typeable a => GFromRel (K1 i a) where
  gFromRel (x:xs) = (K1 (fromDyn x (error "impossible")), xs)
  gFromRel _      = error "impossible: too few elements to gFromRel"

instance {-# OVERLAPS #-} (Typeable a, Generic a, GFromRel (Rep a)) => GFromRel (K1 i (Nested a)) where
  gFromRel xs = (K1 $ Nested (to x), xs')
    where (x, xs') = gFromRel xs
  gFromRel _  = error "impossible: too few elements to gFromRel"

instance {-# OVERLAPS #-} Typeable a => GFromRel (K1 i (Maybe a)) where
  gFromRel (x:xs) = (K1 (fromDyn x (error "impossible")), xs)
  gFromRel _      = error "impossible: too few elements to gFromRel"

instance {-# OVERLAPS #-} Typeable a => GFromRel (K1 i [a]) where
  gFromRel (x:xs) = (K1 (fromDyn x (error "impossible")), xs)
  gFromRel _      = error "impossible: too few elements to gFromRel"

instance GFromRel a => GFromRel (M1 t c a) where
  gFromRel xs = (M1 x, xs')
    where (x, xs') = gFromRel xs
