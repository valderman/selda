{-# LANGUAGE OverloadedStrings, TypeOperators, DeriveGeneric, CPP #-}
-- | Tables for reuse by most tests, and functions for their setup and teardown.
module Tables where
import Database.Selda
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid
#endif

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Text
  , cash :: Double
  } deriving (Generic, Show, Ord, Eq)
instance SqlRow Person

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

setup :: SeldaT IO ()
setup = do
  createTable people
  createTable modPeople
  createTable addresses
  createTable comments
  insert_ (modPeople) peopleItems
  insert_ people peopleItems
  insert_ addresses addressItems
  insert_ comments commentItems

teardown :: SeldaT IO ()
teardown = do
  tryDropTable people
  tryDropTable modPeople
  tryDropTable addresses
  tryDropTable comments
