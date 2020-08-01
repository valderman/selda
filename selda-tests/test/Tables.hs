{-# LANGUAGE OverloadedStrings, TypeOperators, DeriveGeneric, CPP #-}
#if MIN_VERSION_base(4, 9, 0)
{-# LANGUAGE OverloadedLabels, FlexibleContexts, DataKinds, MonoLocalBinds #-}
#endif
-- | Tables for reuse by most tests, and functions for their setup and teardown.
module Tables where
import Database.Selda
import Database.Selda.MakeSelectors

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Text
  , cash :: Double
  } deriving (Generic, Show, Ord, Eq)
instance SqlRow Person

modPeople :: Table Person
modPeople = tableFieldMod "modpeople" [Single pName :- primary] $ \name ->
  "mod_" <> name

people :: Table Person
people = table "people"
  [ Single pName :- primary
  , Single pName :- index
  , Single pCash :- indexUsing HashIndex
  , pName :+ Single pCash :- index
  ]

pName = #name :: Selector Person Text
pAge :: HasField "age" t => Selector t (FieldType "age" t)
pAge  = #age
pPet  = #pet  :: Selector Person (Maybe Text)
pCash = #cash :: HasField "cash" t => Selector t (FieldType "cash" t)

addresses :: Table (Text, Text)
(addresses, aName :*: aCity) = tableWithSelectors "addresses" []

comments, weakComments :: Table (RowID, Maybe Text, Text)
comments = table "comments" [cId :- untypedAutoPrimary]
weakComments = table "comments" [cId :- weakUntypedAutoPrimary]
cId :*: cName :*: cComment = selectors comments

peopleItems =
  [ Person "Link"      125 (Just "horse")  13506
  , Person "Velvet"    19  Nothing         5.55
  , Person "Kobayashi" 23  (Just "dragon") 103707.55
  , Person "Miyu"      10  Nothing         (-500)
  ]

addressItems =
  [ ("Link"      , "Kakariko")
  , ("Kobayashi" , "Tokyo")
  , ("Miyu"      , "Fuyukishi")
  ]

commentItems =
  [ (def, Just "Link" , "Well, excuuuse me, princess!")
  , (def, Nothing     , "Anonymous spam comment")
  ]

resetup :: MonadSelda m => m ()
resetup = do
  tryCreateTable people
  tryCreateTable modPeople
  tryCreateTable addresses
  tryCreateTable comments

setup :: MonadSelda m => m ()
setup = do
  createTable people
  createTable modPeople
  createTable addresses
  createTable comments
  insert_ (modPeople) peopleItems
  insert_ people peopleItems
  insert_ addresses addressItems
  insert_ comments commentItems

teardown :: MonadSelda m => m ()
teardown = do
  tryDropTable people
  tryDropTable modPeople
  tryDropTable addresses
  tryDropTable comments
