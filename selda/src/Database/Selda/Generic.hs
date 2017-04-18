{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- | Build tables and database operations from (almost) any Haskell type.
module Database.Selda.Generic
  ( Generic, GenTable (..), Attribute, Relation
  , genTable, toRel, (!)
  , insertGen, insertGen_, insertGenWithPK
  , primary
  ) where
import Control.Monad.State
import Data.Proxy
import Data.Text (pack)
import GHC.Generics hiding (R, (:*:))
import qualified GHC.Generics as G ((:*:)(..))
import Unsafe.Coerce
import Database.Selda hiding (autoPrimary, primary, optional)
import Database.Selda.Column
import Database.Selda.Table hiding (autoPrimary, primary, optional)
import Database.Selda.SqlType

-- | A generic table. Needs to be unpacked using @gen@ before use with
--   'select', 'insert', etc.
newtype GenTable a = GenTable {gen :: Table (Rel (Rep a))}

-- | Convert a generic type into the corresponding database relation.
--   A type's corresponding relation is simply the inductive tuple consisting
--   of all of the type's fields.
--
-- > data Person = Person
-- >   { id   :: Auto Int
-- >   , name :: Text
-- >   , age  :: Int
-- >   , pet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > somePerson = Person 0 "Velvet" 19 Nothing
-- > (theId :*: theName :*: theAge :*: thePet) = toRel somePerson
toRel :: (Generic a, GRelation (Rep a)) => a -> Rel (Rep a)
toRel = gToRel . from

class NoAuto a where
  noAuto :: a -> InsertCols a
instance NoAuto b => NoAuto (Auto a :*: b) where
  noAuto (_ :*: b) = noAuto b
instance InsertCols (a :*: Auto b) ~ a => NoAuto (a :*: Auto b) where
  noAuto (a :*: _) = a
instance {-# OVERLAPPABLE #-} ((InsertCols (a :*: b)) ~ (a :*: InsertCols b), NoAuto b) => NoAuto (a :*: b) where
  noAuto (a :*: b) = a :*: noAuto b
instance {-# OVERLAPPABLE #-} InsertCols a ~ a => NoAuto a where
  noAuto a = a

-- | Like 'insertWithPK', but accepts a generic table and
--   its corresponding data type.
insertGenWithPK :: ( NoAuto (Rel (Rep a))
                   , Generic a
                   , GRelation (Rep a)
                   , MonadSelda m
                   , HasAutoPrimary (Rel (Rep a))
                   , Insert (InsertCols (Rel (Rep a)))
                   )
                => GenTable a -> [a] -> m Int
insertGenWithPK t = insertWithPK (gen t) . map (noAuto . toRel)

-- | Like 'insert', but accepts a generic table and its corresponding data type.
insertGen :: ( NoAuto (Rel (Rep a))
             , Generic a
             , GRelation (Rep a)
             , MonadSelda m
             , Insert (InsertCols (Rel (Rep a)))
             )
          => GenTable a -> [a] -> m Int
insertGen t = insert (gen t) . map (noAuto . toRel)

-- | Like 'insert_', but accepts a generic table and its corresponding data type.
insertGen_ :: ( NoAuto (Rel (Rep a))
              , Generic a
              , GRelation (Rep a)
              , MonadSelda m
              , Insert (InsertCols (Rel (Rep a)))
              )
           => GenTable a -> [a] -> m ()
insertGen_ t = void . insertGen t

-- | From the given table column, get the column corresponding to the given
--   selector function. For instance:
--
-- > data Person = Person
-- >   { id   :: Auto Int
-- >   , name :: Text
-- >   , age  :: Int
-- >   , pet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > people :: Table Person
-- > people = genTable "people" [name :- primary]
-- >
-- > getAllAges :: Query s Int
-- > getAllAges = do
-- >   p <- select people
-- >   return (p ! age)
--
--   Note that ONLY selector functions may be passed as the second argument of
--   this function. Attempting to pass any non-selector function results in a
--   runtime error.
(!) :: (Columns (Cols s (Rel (Rep a))), Generic a, GRelation (Rep a), SqlType b)
    => Cols s (Rel (Rep a)) -> (a -> b) -> Col s b
cs ! f =
    case drop (identify mkDummy f) cols of
      (Named x _ : _) -> C (Col x)
      (Some c : _)    -> C (unsafeCoerce c)
      _               -> error "attempted to use a non-selector with (!)"
  where
    cols = fromTup cs

-- | Some attribute that may be set on a table column.
newtype Attribute = Attribute [ColAttr]

-- | A primary key which does not auto-increment.
primary :: Attribute
primary = Attribute [Primary, Required]

-- | A dummy of some type. Encapsulated to avoid improper use, since all of
--   its fields are 'unsafeCoerce'd ints.
newtype Dummy a = Dummy a

-- | The relation corresponding to the given Haskell type.
--   This relation simply corresponds to the fields in the data type, from
--   left to right. For instance:
--
-- > data Foo = Foo
-- >   { bar :: Int
-- >   , baz :: Text
-- >   }
--
--   In this example, @Relation Foo@ is @(Int :*: Text)@, aas the first field
--   of @Foo@ has type @Int@, and the second has type @Text@.
type Relation a = Rel (Rep a)

-- | Generate a table from the given table name and list of column attributes.
--   All @Maybe@ fields in the table's type will be represented by nullable
--   columns, and all non-@Maybe@ fields fill be represented by required
--   columns.
--   If the type has an auto-incrementing primary key, the primary key must
--   have the type @Auto Int@.
--   For example:
--
-- > data Person = Person
-- >   { id   :: Auto Int
-- >   , name :: Text
-- >   , age  :: Int
-- >   , pet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > people :: Table Person
-- > people = genTable "people" [name :- primary]
--
--   This example will create a table with the column types
--   @Auto Int :*: Text :*: Int :*: Maybe Text@, where the first field is
--   the primary key.
genTable :: forall a b.
            (Generic a, GRelation (Rep a))
         => TableName
         -> [(a -> b, Attribute)]
         -> GenTable a
genTable tn attrs = GenTable $ Table tn (validate tn (map tidy cols))
  where
    dummy = mkDummy
    cols = zipWith addAttrs [0..] (tblCols (Proxy :: Proxy a))
    addAttrs n ci = ci
      { colAttrs = colAttrs ci ++ concat
          [ as
          | (f, Attribute as) <- attrs
          , identify dummy f == n
          ]
      }

-- | Extract all column names from the given type.
--   If the type is not a record, the columns will be named @col_1@,
--   @col_2@, etc.
tblCols :: forall a. (GRelation (Rep a)) => Proxy a -> [ColInfo]
tblCols _ = zipWith pack' [0 :: Int ..] $ gTblCols (Proxy :: Proxy (Rep a))
  where
    pack' n ci = ci
      { colName = if colName ci == ""
                    then pack $ "col_" ++ show n
                    else colName ci
      }

-- | Create a dummy of the given type.
mkDummy :: (Generic a, GRelation (Rep a)) => Dummy a
mkDummy = Dummy $ to $ evalState gMkDummy 0

-- | Get the selector identifier of the given selector for the given dummy.
identify :: Dummy a -> (a -> b) -> Int
identify (Dummy d) f = unsafeCoerce $ f d

class Traits a where
  isMaybeType :: Proxy a -> Bool
  isMaybeType _ = False
  isAutoType :: Proxy a -> Bool
  isAutoType _ = False
instance Traits (Maybe a) where
  isMaybeType _ = True
instance Traits (Auto Int) where
  isAutoType _ = True
instance {-# OVERLAPPABLE #-} Traits a

-- | Normalized append of two inductive tuples.
--   Note that this will flatten any nested inductive tuples.
type family a :++: b where
  (a :*: b) :++: c = a :*: (b :++: c)
  a         :++: b = a :*: b

class Append a b where
  app :: a -> b -> a :++: b

instance {-# OVERLAPPING #-} Append b c => Append (a :*: b) c where
  app (a :*: b) c = a :*: app b c

instance ((a :*: b) ~ (a :++: b)) => Append a b where
  app a b = a :*: b

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

instance (Selector c, GRelation a) => GRelation (M1 S c a) where
  gToRel (M1 x) = gToRel x
  gTblCols _    = [ci']
    where
      [ci] = gTblCols (Proxy :: Proxy a)
      ci' = ColInfo
        { colName = pack $ selName ((M1 undefined) :: M1 S c a b)
        , colType = colType ci
        , colAttrs = colAttrs ci
        }
  gMkDummy = M1 <$> gMkDummy

instance (Traits a, SqlType a) => GRelation (K1 i a) where
  gToRel (K1 x) = x
  gTblCols _    = [ColInfo "" (sqlType (Proxy :: Proxy a)) attrs]
    where
      attrs = auto ++ optReq
      auto
        | isAutoType (Proxy :: Proxy a) = [Primary, AutoIncrement]
        | otherwise                     = []
      optReq
        | isMaybeType (Proxy :: Proxy a) = [Optional]
        | otherwise                      = [Required]
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
