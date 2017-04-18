{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- | Build tables and database operations from (almost) any Haskell type.
module Database.Selda.Generic
  ( ColAttribute (..), Attribute, Relation
  , genTable
  , primary, autoPrimary, Person (..)
  ) where
import Control.Monad.State
import Data.Proxy
import Data.Text (pack)
import GHC.Generics hiding (R, (:*:))
import qualified GHC.Generics as G ((:*:)(..))
import Unsafe.Coerce
import Database.Selda hiding (autoPrimary, primary, optional)
import Database.Selda.Table hiding (autoPrimary, primary, optional)
import Database.Selda.SqlType

-- | Some attribute that may be set on a table column.
newtype Attribute = Attribute [ColAttr]

-- | Auto-incrementing primary key.
autoPrimary :: Attribute
autoPrimary = Attribute [Primary, AutoIncrement, Required]

-- | A primary key which does not auto-increment.
primary :: Attribute
primary = Attribute [Primary, Required]

-- | A dummy of some type. Encapsulated to avoid improper use, since all of
--   its fields are 'unsafeCoerce'd ints.
newtype Dummy a = Dummy a

-- | A pair of columns and attributes.
data ColAttribute a where
  (:-) :: (a -> b) -> Attribute -> ColAttribute a

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
--   For example:
--
-- > data Person = Person
-- >   { name :: Text
-- >   , age  :: Int
-- >   , pet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > people :: Table Person
-- > people = genTable "people" [name :- primary]
--
--   This example will create a table with the column types
--   @Text :*: Int :*: Maybe Text@, where the first field is the primary key.
--
--   TODO: auto-incrementing columns don't have any particular type here, find
--   out just how big/small a problem this is.
genTable :: forall a.
            (Generic a, GRelation (Rep a))
         => TableName
         -> [ColAttribute a]
         -> Table a
genTable tn attrs = Table tn (validate tn (map tidy cols))
  where
    dummy = mkDummy
    cols = zipWith addAttrs [0..] (tblCols (Proxy :: Proxy a))
    addAttrs n ci = ci
      { colAttrs = colAttrs ci ++ concat
          [ as
          | f :- (Attribute as) <- attrs
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

class MaybeMaybe a where
  isMaybeType :: Proxy a -> Bool
instance MaybeMaybe (Maybe a) where
  isMaybeType _ = True
instance {-# OVERLAPPABLE #-} MaybeMaybe a where
  isMaybeType _ = False

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

instance (MaybeMaybe a, SqlType a) => GRelation (K1 i a) where
  gToRel (K1 x) = x
  gTblCols _    = [ColInfo "" (sqlType (Proxy :: Proxy a)) attrs]
    where
      attrs
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
