{-# LANGUAGE OverloadedStrings, TypeOperators, DeriveGeneric #-}
-- | Tables for reuse by most tests, and functions for their setup and teardown.
module Tables where
import Database.Selda
import Database.Selda.Generic

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Text
  , cash :: Double
  } deriving (Generic, Show, Ord, Eq)

genPeople :: GenTable Person
genPeople = genTable "genpeople" [name :- primaryGen, name :- indexGen]

genModPeople :: GenTable Person
genModPeople = genTableFieldMod "genmodpeople" [name :- primaryGen] $ \name ->
  "genmod_" ++ name

people :: Table (Text :*: Int :*: Maybe Text :*: Double)
people =
      table "people"
  $   primary "name"
  :*: required "age"
  :*: indexed (optional "pet")
  :*: indexedUsing HashIndex (required "cash")
pName :*: pAge :*: pPet :*: pCash = selectors people

addresses :: Table (Text :*: Text)
(addresses, aName :*: aCity) =
      tableWithSelectors "addresses"
  $   required "name"
  :*: required "city"

comments :: Table (RowID :*: Maybe Text :*: Text)
comments =
      table "comments"
  $   autoPrimary "id"
  :*: optional "author"
  :*: required "comment"
cId :*: cName :*: cComment = selectors comments

genPeopleItems =
  [ Person "Link"      125 (Just "horse")  13506
  , Person "Velvet"     19 Nothing         5.55
  , Person "Kobayashi"  23 (Just "dragon") 103707.55
  , Person "Miyu"       10 Nothing         (-500)
  ]

peopleItems =
  [ "Link"      :*: 125 :*: Just "horse"  :*: 13506
  , "Velvet"    :*: 19  :*: Nothing       :*: 5.55
  , "Kobayashi" :*: 23  :*: Just "dragon" :*: 103707.55
  , "Miyu"      :*: 10  :*: Nothing       :*: (-500)
  ]

addressItems =
  [ "Link"      :*: "Kakariko"
  , "Kobayashi" :*: "Tokyo"
  , "Miyu"      :*: "Fuyukishi"
  ]

commentItems =
  [ Just "Link" :*: "Well, excuuuse me, princess!"
  , Nothing     :*: "Anonymous spam comment"
  ]

setup :: SeldaT IO ()
setup = do
  createTable (gen genPeople)
  createTable (gen genModPeople)
  createTable people
  createTable addresses
  createTable comments
  insert_ (gen genPeople) peopleItems
  insert_ (gen genModPeople) peopleItems
  insert_ people peopleItems
  insert_ addresses addressItems
  insert_ comments (map (def :*:) commentItems)

teardown :: SeldaT IO ()
teardown = do
  tryDropTable (gen genPeople)
  tryDropTable (gen genModPeople)
  tryDropTable people
  tryDropTable addresses
  tryDropTable comments
