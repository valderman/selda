{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
-- | Build tables and database operations from (almost) any Haskell type.
--
--   While the types in this module may look somewhat intimidating, the rules
--   for generic tables and queries are quite simple:
--
--     * Any record type with a single data constructor, where all fields are
--       instances of 'SqlType', can be used for generic tables and queries
--       if it derives 'Generic'.
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
  ( Relational, Generic
  , GenAttr (..), GenTable (..), Attribute, Relation
  , genTable, toRel, toRels, fromRel, fromRels
  , insertGen, insertGen_, insertGenWithPK
  , primaryGen, autoPrimaryGen, uniqueGen, fkGen
  ) where
import Control.Monad.State
import Data.Dynamic
import Data.Proxy
import Data.Text (pack)
import Data.Typeable
import GHC.Generics hiding (R, (:*:), Selector)
import qualified GHC.Generics as G ((:*:)(..), Selector)
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

-- | A generic table. Needs to be unpacked using @gen@ before use with
--   'select', 'insert', etc.
newtype GenTable a = GenTable {gen :: Table (Relation a)}

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
genTable tn attrs = GenTable $ Table tn (validate tn (map tidy cols)) apk
  where
    dummy = mkDummy
    cols = zipWith addAttrs [0..] (tblCols (Proxy :: Proxy a))
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
fromRel :: Relational a => Relation a -> a
fromRel = to . fst . gFromRel . toDyns

-- | Convenient synonym for @map fromRel@.
fromRels :: Relational a => [Relation a] -> [a]
fromRels = map fromRel

-- | Like 'insertWithPK', but accepts a generic table and
--   its corresponding data type.
insertGenWithPK :: (Relational a, MonadSelda m) => GenTable a -> [a] -> m RowID
insertGenWithPK t = insertWithPK (gen t) . map toRel

-- | Like 'insert', but accepts a generic table and its corresponding data type.
insertGen :: (Relational a, MonadSelda m) => GenTable a -> [a] -> m Int
insertGen t = insert (gen t) . map toRel

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
tblCols :: forall a. (GRelation (Rep a)) => Proxy a -> [ColInfo]
tblCols _ = zipWith pack' [0 :: Int ..] $ gTblCols (Proxy :: Proxy (Rep a))
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

-- | The relation corresponding to the given type.
type family Rel (rep :: * -> *) where
  Rel (M1 t c a)  = Rel a
  Rel (K1 i a)    = a
  Rel (a G.:*: b) = Rel a :++: Rel b

class GRelation f where
  -- | Convert a value from its Haskell type into the corresponding relation.
  gToRel   :: f a -> Rel f

  -- | Compute all columns needed to represent the given type.
  gTblCols :: Proxy f -> [ColInfo]

  -- | Create a dummy value where all fields are replaced by @unsafeCoerce@'d
  --   ints. See 'mkDummy' and 'identify' for more information.
  gMkDummy :: State Int (f a)

instance GRelation a => GRelation (M1 C c a) where
  gToRel (M1 x)   = gToRel x
  gTblCols _ = gTblCols (Proxy :: Proxy a)
  gMkDummy = M1 <$> gMkDummy

instance GRelation a => GRelation (M1 D c a) where
  gToRel (M1 x)   = gToRel x
  gTblCols _ = gTblCols (Proxy :: Proxy a)
  gMkDummy = M1 <$> gMkDummy

instance (G.Selector c, GRelation a) => GRelation (M1 S c a) where
  gToRel (M1 x) = gToRel x
  gTblCols _    = [ci']
    where
      [ci] = gTblCols (Proxy :: Proxy a)
      ci' = ColInfo
        { colName = mkColName . pack $ selName ((M1 undefined) :: M1 S c a b)
        , colType = colType ci
        , colAttrs = colAttrs ci
        , colFKs = colFKs ci
        }
  gMkDummy = M1 <$> gMkDummy

instance (Typeable a, SqlType a) => GRelation (K1 i a) where
  gToRel (K1 x) = x
  gTblCols _    = [ColInfo "" (sqlType (Proxy :: Proxy a)) optReq []]
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

instance (Append (Rel a) (Rel b), GRelation a, GRelation b) =>
         GRelation (a G.:*: b) where
  gToRel (a G.:*: b)   = gToRel a `app` gToRel b
  gTblCols _ = gTblCols a ++ gTblCols b
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

instance GFromRel a => GFromRel (M1 t c a) where
  gFromRel xs = (M1 x, xs')
    where (x, xs') = gFromRel xs
