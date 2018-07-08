{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GADTs, CPP, DeriveGeneric, DataKinds #-}
-- | Generics utilities.
module Database.Selda.Generic
  ( Relational, Generic, FromRel, ToDyn
  , Relation, Relations, Nested (..)
  , tblCols, mkDummy, identify, params
  , def, fromRel, fromRels, toRel, toRels
  ) where
import Control.Monad.State
import Data.Dynamic
import Data.Text as Text (Text, pack, intercalate, unwords, empty)
#if MIN_VERSION_base(4, 10, 0)
import Data.Typeable
#endif
import GHC.Generics hiding (R, (:*:), Selector)
import qualified GHC.Generics as G ((:*:)(..), (:+:)(..), Selector, R)
#if MIN_VERSION_base(4, 9, 0)
import qualified GHC.TypeLits as TL
#endif
import Unsafe.Coerce
import Control.Exception (Exception (..), try, throw)
import System.IO.Unsafe
import Database.Selda.Types
import Database.Selda.SqlType
import Database.Selda.Table.Type
import Database.Selda.SQL (Param (..))

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

-- | A dummy of some type. Encapsulated to avoid improper use, since all of
--   its fields are 'unsafeCoerce'd ints.
newtype Dummy a = Dummy a

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

-- | Extract all insert parameters from a generic value.
params :: Relational a => a -> [Either Param Param]
params = unsafePerformIO . gParams . from

-- | Extract all column names from the given type.
--   If the type is not a record, the columns will be named @col_1@,
--   @col_2@, etc.
tblCols :: forall a. Relational a => Proxy a -> (Text -> Text) -> [ColInfo]
tblCols _ fieldMod =
    zipWith pack' [0 :: Int ..] $ gTblCols (Proxy :: Proxy (Rep a))
  where
    pack' n ci = ci
      { colName = if colName ci == ""
                    then mkColName $ fieldMod ("col_" <> pack (show n))
                    else modColName (colName ci) fieldMod
      }

-- | Exception indicating the use of a default value.
--   If any values throwing this during evaluation of @param xs@ will be
--   replaced by their default value.
data DefaultValueException = DefaultValueException
  deriving Show
instance Exception DefaultValueException

-- | The default value for a column during insertion.
--   For an auto-incrementing primary key, the default value is the next key.
--
--   Using @def@ in any other context than insertion results in a runtime error.
def :: SqlType a => a
def = throw DefaultValueException

class GRelation f where
  -- | Convert a value from its Haskell type into the corresponding relation.
  gToRel   :: f a -> Rel f

  -- | Generic worker for 'params'.
  gParams :: f a -> IO [Either Param Param]

  -- | Compute all columns needed to represent the given type.
  gTblCols :: Proxy f -> [ColInfo]

  -- | Create a dummy value where all fields are replaced by @unsafeCoerce@'d
  --   ints. See 'mkDummy' and 'identify' for more information.
  gMkDummy :: State Int (f a)

instance {-# OVERLAPPABLE #-} GRelation a => GRelation (M1 t c a) where
  gToRel (M1 x) = gToRel x
  gParams (M1 x) = gParams x
  gTblCols _ = gTblCols (Proxy :: Proxy a)
  gMkDummy = M1 <$> gMkDummy

instance {-# OVERLAPPING #-} (G.Selector c, GRelation a) =>
         GRelation (M1 S c a) where
  gToRel (M1 x) = gToRel x
  gParams (M1 x) = gParams x
  gTblCols _ = colinfos'
    where
      colinfos = gTblCols (Proxy :: Proxy a)
      colinfos' =
        case colinfos of
          [ci] -> [ColInfo
            { colName = mkColName $ pack (selName ((M1 undefined) :: M1 S c a b))
            , colType = colType ci
            , colAttrs = colAttrs ci
            , colFKs = colFKs ci
            }]
          _ -> colinfos
  gMkDummy = M1 <$> gMkDummy

instance (Rel (K1 i a) ~ a, Typeable a, SqlType a) => GRelation (K1 i a) where
  gToRel (K1 x) = x

  gParams (K1 x) = do
    res <- try $ return $! x
    return $ case res of
      Right x'                   -> [Right $ Param (mkLit x')]
      Left DefaultValueException -> [Left $ Param (defaultValue :: Lit a)]

  gTblCols _ = [ColInfo "" (sqlType (Proxy :: Proxy a)) optReq []]
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
  gParams (K1 (Nested x)) = gParams (from x)
  gTblCols _ = gTblCols (Proxy :: Proxy (Rep a))
  gMkDummy = fmap (K1 . Nested . to) gMkDummy

instance (Append (Rel a) (Rel b), GRelation a, GRelation b) =>
         GRelation (a G.:*: b) where
  gToRel (a G.:*: b) = gToRel a `app` gToRel b
  gParams (a G.:*: b) = liftM2 (++) (gParams a) (gParams b)
  gTblCols _ = gTblCols a ++ gTblCols b
    where
      a = Proxy :: Proxy a
      b = Proxy :: Proxy b
  gMkDummy = do
    a <- gMkDummy :: State Int (a x)
    b <- gMkDummy :: State Int (b x)
    return (a G.:*: b)

#if MIN_VERSION_base(4, 9, 0)
instance
  (TL.TypeError
    ( 'TL.Text "Selda currently does not support creating tables from sum types."
      'TL.:$$:
      'TL.Text "Restrict your table type to a single data constructor."
    )) => GRelation (a G.:+: b) where
  gToRel = error "unreachable"
  gTblCols = error "unreachable"
  gMkDummy = error "unreachable"
#endif

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

instance {-# OVERLAPS #-} (Typeable a, Generic a, GFromRel (Rep a)) =>
                          GFromRel (K1 i (Nested a)) where
  gFromRel xs = (K1 $ Nested (to x), xs')
    where (x, xs') = gFromRel xs

instance {-# OVERLAPS #-} Typeable a => GFromRel (K1 i (Maybe a)) where
  gFromRel (x:xs) = (K1 (fromDyn x (error "impossible")), xs)
  gFromRel _      = error "impossible: too few elements to gFromRel"

instance {-# OVERLAPS #-} Typeable a => GFromRel (K1 i [a]) where
  gFromRel (x:xs) = (K1 (fromDyn x (error "impossible")), xs)
  gFromRel _      = error "impossible: too few elements to gFromRel"

instance GFromRel a => GFromRel (M1 t c a) where
  gFromRel xs = (M1 x, xs')
    where (x, xs') = gFromRel xs
