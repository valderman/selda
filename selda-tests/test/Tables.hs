{-# LANGUAGE OverloadedStrings, TypeOperators, DeriveGeneric #-}
-- | Tables for reuse by most tests, and functions for their setup and teardown.
module Tables where
import Database.Selda

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Text
  , cash :: Double
  } deriving (Generic, Show, Ord, Eq)

modPeople :: Table Person
modPeople = tableFieldMod "modpeople" [name :- primary] $ \name ->
  "mod_" <> name

people :: Table Person
people = table "people"
  [ name :- primary
  , name :- index
  , cash :- indexUsing HashIndex
  ]
pName :*: pAge :*: pPet :*: pCash = selectors people

addresses :: Table (Text, Text)
(addresses, aName :*: aCity) = tableWithSelectors "addresses" []

comments :: Table (RowID, Maybe Text, Text)
comments = table "comments" [(\(x,_,_) -> x) :- untypedAutoPrimary]
cId :*: cName :*: cComment = selectors comments

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
  createTable people
  createTable modPeople
  createTable addresses
  createTable comments
  insert_ (modPeople) (fromRels peopleItems)
  insert_ people (fromRels peopleItems)
  insert_ addresses (fromRels addressItems)
  insert_ comments (fromRels (map (def :*:) commentItems))

teardown :: SeldaT IO ()
teardown = do
  tryDropTable people
  tryDropTable modPeople
  tryDropTable addresses
  tryDropTable comments
