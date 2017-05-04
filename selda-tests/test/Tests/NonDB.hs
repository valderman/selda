-- | Misc. tests that don't touch the database.
module Tests.NonDB where
import Data.List hiding (groupBy, insert)
import Database.Selda
import Database.Selda.Generic
import Test.HUnit
import Utils
import Tables

noDBTests = test
  [ "id == fromRel . toRel" ~: fromRelToRelId
  ]

fromRelToRelId =
    assertEqual "fromRel . toRel /= id" genPeopleItems xs
  where
    xs = map (fromRel . toRel) genPeopleItems
