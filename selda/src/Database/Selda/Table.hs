{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GADTs, CPP, DeriveGeneric, DataKinds #-}
module Database.Selda.Table
  ( Attr (..), Table (..), Attribute
  , ColInfo (..), ColAttr (..), IndexMethod (..)
  , ForeignKey (..)
  , table, tableFieldMod -- , tableWithSelectors, selectors
  , primary, autoPrimary, untypedAutoPrimary, unique
  , index, indexUsing
  , tableExpr
  ) where
import Data.Text (Text)
#if MIN_VERSION_base(4, 10, 0)
import Data.Typeable
#else
import Data.Proxy
#endif
import Database.Selda.Types
import Database.Selda.Selectors
import Database.Selda.SqlType
import Database.Selda.Column (Row (..))
import Database.Selda.Generic
import Database.Selda.Table.Type
import Database.Selda.Table.Validation (snub)

-- | A generic column attribute.
--   Essentially a pair or a record selector over the type @a@ and a column
--   attribute.
data Attr a where
  (:-) :: Selector a b -> Attribute a b -> Attr a

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
-- > people = table "people" [pId :- autoPrimary]
-- > pId :*: pName :*: pAge :*: pPet = selectors people
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
-- > people = tableFieldMod "people" [personName :- autoPrimaryGen] (stripPrefix "person")
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
  }
  where
    cols = zipWith addAttrs [0..] (tblCols (Proxy :: Proxy a) fieldMod)
    apk = or [AutoIncrement `elem` as | _ :- Attribute as <- attrs]
    addAttrs n ci = ci
      { colAttrs = colAttrs ci ++ concat
          [ as
          | sel :- Attribute as <- attrs
          , selectorIndex sel == n
          ]
      , colFKs = colFKs ci ++
          [ thefk
          | sel :- ForeignKey thefk <- attrs
          , selectorIndex sel == n
          ]
      }

-- | Remove duplicate attributes.
tidy :: ColInfo -> ColInfo
tidy ci = ci {colAttrs = snub $ colAttrs ci}

-- | Some attribute that may be set on a column of type @c@, in a table of
--   type @t@.
data Attribute t c
  = Attribute [ColAttr]
  | ForeignKey (Table (), ColName)

-- | A primary key which does not auto-increment.
primary :: Attribute t c
primary = Attribute [Primary, Required, Unique]

-- | Create an index on this column.
index :: Attribute t c
index = Attribute [Indexed Nothing]

-- | Create an index using the given index method on this column.
indexUsing :: IndexMethod -> Attribute t c
indexUsing m = Attribute [Indexed (Just m)]

-- | An auto-incrementing primary key.
autoPrimary :: Attribute t (ID t)
autoPrimary = Attribute [Primary, AutoIncrement, Required, Unique]

-- | An untyped auto-incrementing primary key.
--   You should really only use this for ad hoc tables, such as tuples.
untypedAutoPrimary :: Attribute t RowID
untypedAutoPrimary = Attribute [Primary, AutoIncrement, Required, Unique]

-- | A table-unique value.
unique :: Attribute t c
unique = Attribute [Unique]

mkFK :: Table t -> Selector a b -> Attribute c d
mkFK (Table tn tcs tapk) sel =
  ForeignKey (Table tn tcs tapk, colName (tcs !! selectorIndex sel))

class ForeignKey a b where
  -- | A foreign key constraint referencing the given table and column.
  foreignKey :: Table t -> Selector t a -> Attribute self b

instance ForeignKey a a where
  foreignKey = mkFK
instance ForeignKey (Maybe a) a where
  foreignKey = mkFK
instance ForeignKey a (Maybe a) where
  foreignKey = mkFK

-- | An expression representing the given table.
tableExpr :: Table a -> Row s a
tableExpr = Many . map colExpr . tableCols
