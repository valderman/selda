{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GADTs, CPP, DeriveGeneric, DataKinds, MagicHash #-}
#if MIN_VERSION_base(4, 10, 0)
{-# LANGUAGE TypeApplications #-}
#endif
module Database.Selda.Table
  ( SelectorLike, Group (..), Attr (..), Table (..), Attribute
  , ColInfo (..), AutoIncType (..), ColAttr (..), IndexMethod (..)
  , ForeignKey (..)
  , table, tableFieldMod
  , primary, autoPrimary, weakAutoPrimary
  , untypedAutoPrimary, weakUntypedAutoPrimary
  , unique
  , index, indexUsing
  , tableExpr
  , isAutoPrimary, isPrimary, isUnique
  ) where
import Data.Text (Text)
#if MIN_VERSION_base(4, 10, 0)
import Data.Typeable
#else
import Data.Proxy
import GHC.Prim
#endif
import Database.Selda.Types
import Database.Selda.Selectors
import Database.Selda.SqlType
import Database.Selda.Column (Row (..))
import Database.Selda.Generic
import Database.Selda.Table.Type
import Database.Selda.Table.Validation (snub)
import GHC.OverloadedLabels

instance forall x t a. IsLabel x (Selector t a) => IsLabel x (Group t a) where
#if MIN_VERSION_base(4, 10, 0)
  fromLabel = Single (fromLabel @x)
#else
  fromLabel _ = Single (fromLabel (proxy# :: Proxy# x))
#endif

-- | A non-empty list of selectors, where the element selectors need not have
--   the same type. Used to specify constraints, such as uniqueness or primary
--   key, potentially spanning multiple columns.
data Group t a where
  (:+)   :: Selector t a -> Group t b -> Group t (a :*: b)
  Single :: Selector t a -> Group t a
infixr 1 :+

-- | A generic column attribute.
--   Essentially a pair or a record selector over the type @a@ and a column
--   attribute. An attribute may be either a 'Group' attribute, meaning that
--   it can span multiple columns, or a 'Selector' -- single column -- attribute.
data Attr a where
  (:-) :: SelectorLike g => g t a -> Attribute g t a -> Attr t
infixl 0 :-

-- | Generate a table from the given table name and list of column attributes.
--   All @Maybe@ fields in the table's type will be represented by nullable
--   columns, and all non-@Maybe@ fields fill be represented by required
--   columns.
--   For example:
--
-- > data Person = Person
-- >   { id   :: ID Person
-- >   , name :: Text
-- >   , age  :: Int
-- >   , pet  :: Maybe Text
-- >   }
-- >   deriving Generic
-- >
-- > people :: Table Person
-- > people = table "people" [#id :- autoPrimary]
--
--   This will result in a table of @Person@s, with an auto-incrementing primary
--   key.
--
--   If the given type does not have record selectors, the column names will be
--   @col_1@, @col_2@, etc.
table :: forall a. Relational a
         => TableName
         -> [Attr a]
         -> Table a
table tn attrs = tableFieldMod tn attrs id

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
-- > people :: Table Person
-- > people = tableFieldMod "people"
-- >   [#personName :- autoPrimaryGen]
-- >   (fromJust . stripPrefix "person")
--
--   This will create a table with the columns named
--   @Id@, @Name@, @Age@ and @Pet@.
tableFieldMod :: forall a. Relational a
                 => TableName
                 -> [Attr a]
                 -> (Text -> Text)
                 -> Table a
tableFieldMod tn attrs fieldMod = Table
  { tableName = tn
  , tableCols = map tidy cols
  , tableHasAutoPK = apk
  , tableAttrs = concat [combinedAttrs, pkAttrs]
  }
  where
    combinedAttrs =
      [ (ixs, a)
      | sel :- Attribute [a] <- attrs
      , let ixs = indices sel
      , case ixs of
          (_:_:_)              -> True
          [_] | a == Unique    -> True
          [_] | Indexed _ <- a -> True
          _                    -> False
      ]
    pkAttrs = concat
      [ [(ixs, Primary), (ixs, Required)]
      | sel :- Attribute [Primary,Required] <- attrs
      , let ixs = indices sel
      ]
    cols = zipWith addAttrs [0..] (tblCols (Proxy :: Proxy a) fieldMod)
    apk = or [any isAutoPrimary as | _ :- Attribute as <- attrs]
    addAttrs n ci = ci
      { colAttrs = colAttrs ci ++ concat
          [ as
          | sel :- Attribute as <- attrs
          , case indices sel of
              [colIx] -> colIx == n
              _       -> False
          ]
      , colFKs = colFKs ci ++
          [ thefk
          | sel :- ForeignKey thefk <- attrs
          , case indices sel of
              [colIx] -> colIx == n
              _       -> False
          ]
      }

class SelectorLike g where
  indices :: g t a -> [Int]

instance SelectorLike Selector where
  indices s = [selectorIndex s]
instance SelectorLike Group where
  indices (s :+ ss)  = selectorIndex s : indices ss
  indices (Single s) = [selectorIndex s]

-- | Remove duplicate attributes.
tidy :: ColInfo -> ColInfo
tidy ci = ci {colAttrs = snub $ colAttrs ci}

-- | Some attribute that may be set on a column of type @c@, in a table of
--   type @t@.
data Attribute (g :: * -> * -> *) t c
  = Attribute [ColAttr]
  | ForeignKey (Table (), ColName)

-- | A primary key which does not auto-increment.
primary :: Attribute Group t a
primary = Attribute [Primary, Required]

-- | Create an index on these column(s).
index :: Attribute Group t c
index = Attribute [Indexed Nothing]

-- | Create an index using the given index method on this column.
indexUsing :: IndexMethod -> Attribute Group t c
indexUsing m = Attribute [Indexed (Just m)]

-- | An auto-incrementing primary key.
autoPrimary :: Attribute Selector t (ID t)
autoPrimary = Attribute [AutoPrimary Strong, Required]

-- | A "weakly auto-incrementing" primary key.
--   Behaves like 'autoPrimary', but the sequence of generated keys is not
--   guaranteed to be monotonically increasing.
--
--   This gives better performance on some backends, but means that
--   the relation @a > b <=> a was inserted at a later point in time than b@
--   does not hold.
weakAutoPrimary :: Attribute Selector t (ID t)
weakAutoPrimary = Attribute [AutoPrimary Weak, Required]

-- | An untyped auto-incrementing primary key.
--   You should really only use this for ad hoc tables, such as tuples.
untypedAutoPrimary :: Attribute Selector t RowID
untypedAutoPrimary = Attribute [AutoPrimary Strong, Required]

-- | Like 'weakAutoPrimary', but for untyped IDs.
weakUntypedAutoPrimary :: Attribute Selector t RowID
weakUntypedAutoPrimary = Attribute [AutoPrimary Weak, Required]

-- | A table-unique value.
unique :: Attribute Group t a
unique = Attribute [Unique]

mkFK :: Table t -> Selector a b -> Attribute Selector c d
mkFK (Table tn tcs tapk tas) sel =
  ForeignKey (Table tn tcs tapk tas, colName (tcs !! selectorIndex sel))

class ForeignKey a b where
  -- | A foreign key constraint referencing the given table and column.
  foreignKey :: Table t -> Selector t a -> Attribute Selector self b

instance ForeignKey a a where
  foreignKey = mkFK
instance ForeignKey (Maybe a) a where
  foreignKey = mkFK
instance ForeignKey a (Maybe a) where
  foreignKey = mkFK

-- | An expression representing the given table.
tableExpr :: Table a -> Row s a
tableExpr = Many . map colExpr . tableCols
