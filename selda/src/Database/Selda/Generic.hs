{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GADTs, CPP, DeriveGeneric, DataKinds #-}
-- | Generics utilities.
module Database.Selda.Generic
  ( Relational, Generic, Nested (..)
  , tblCols, mkDummy, identify, params
  , def
  ) where
import Control.Monad.State
import Data.Dynamic
import Data.Text as Text (Text, pack)
#if MIN_VERSION_base(4, 10, 0)
import Data.Typeable
#endif
import GHC.Generics hiding (R, (:*:), Selector)
import qualified GHC.Generics as G ((:*:)(..), (:+:)(..), Selector)
#if MIN_VERSION_base(4, 9, 0)
import qualified GHC.TypeLits as TL
#endif
import Unsafe.Coerce
import Control.Exception (Exception (..), try, throw)
import System.IO.Unsafe
import Database.Selda.Types
import Database.Selda.Selectors
import Database.Selda.SqlType
import Database.Selda.SqlResult (SqlResult)
import Database.Selda.Table.Type
import Database.Selda.SQL (Param (..))
import Database.Selda.Exp (Exp (Col), UntypedCol (..))

-- | Any type which has a corresponding relation.
--   To make a @Relational@ instance for some type, simply derive 'Generic'.
--
--   Note that only types which have a single data constructor, and where all
--   fields are instances of 'SqlValue' can be used with this module.
--   Attempting to use functions in this module with any type which doesn't
--   obey those constraints will result in a very confusing type error.
type Relational a =
  ( Generic a
  , SqlResult a
  , GRelation (Rep a)
  , GSelectors a (Rep a)
  )

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

-- | Extract all insert parameters from a generic value.
params :: Relational a => a -> [Either Param Param]
params = unsafePerformIO . gParams . from

-- | Extract all column names from the given type.
--   If the type is not a record, the columns will be named @col_1@,
--   @col_2@, etc.
tblCols :: forall a. Relational a => Proxy a -> (Text -> Text) -> [ColInfo]
tblCols _ fieldMod =
    evalState (gTblCols (Proxy :: Proxy (Rep a)) Nothing rename) 0
  where
    rename n Nothing     = mkColName $ fieldMod ("col_" <> pack (show n))
    rename _ (Just name) = modColName name fieldMod

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
  -- | Generic worker for 'params'.
  gParams :: f a -> IO [Either Param Param]

  -- | Compute all columns needed to represent the given type.
  gTblCols :: Proxy f
           -> Maybe ColName
           -> (Int -> Maybe ColName -> ColName)
           -> State Int [ColInfo]

  -- | Create a dummy value where all fields are replaced by @unsafeCoerce@'d
  --   ints. See 'mkDummy' and 'identify' for more information.
  gMkDummy :: State Int (f a)

instance {-# OVERLAPPABLE #-} GRelation a => GRelation (M1 t c a) where
  gParams (M1 x) = gParams x
  gTblCols _ = gTblCols (Proxy :: Proxy a)
  gMkDummy = M1 <$> gMkDummy

instance {-# OVERLAPPING #-} (G.Selector c, GRelation a) =>
         GRelation (M1 S c a) where
  gParams (M1 x) = gParams x
  gTblCols _ _ = gTblCols (Proxy :: Proxy a) (Just name)
    where name = mkColName $ pack (selName ((M1 undefined) :: M1 S c a b))
  gMkDummy = M1 <$> gMkDummy

instance (Typeable a, SqlType a) => GRelation (K1 i a) where
  gParams (K1 x) = do
    res <- try $ return $! x
    return $ case res of
      Right x'                   -> [Right $ Param (mkLit x')]
      Left DefaultValueException -> [Left $ Param (defaultValue :: Lit a)]

  gTblCols _ name rename = do
    n <- get
    put (n+1)
    let name' = rename n name
    return
      [ ColInfo
        { colName = name'
        , colType = sqlType (Proxy :: Proxy a)
        , colAttrs = optReq
        , colFKs = []
        , colExpr = Untyped (Col name')
        }
      ]
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
  , Generic a
  ) => GRelation (K1 i (Nested a)) where
  gParams (K1 (Nested x)) = gParams (from x)
  gTblCols _ = gTblCols (Proxy :: Proxy (Rep a))
  gMkDummy = fmap (K1 . Nested . to) gMkDummy

instance (GRelation a, GRelation b) => GRelation (a G.:*: b) where
  gParams (a G.:*: b) = liftM2 (++) (gParams a) (gParams b)
  gTblCols _ _ rename = do
      as <- gTblCols a Nothing rename
      bs <- gTblCols b Nothing rename
      return (as ++ bs)
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
  gParams = error "unreachable"
  gTblCols = error "unreachable"
  gMkDummy = error "unreachable"
#endif
