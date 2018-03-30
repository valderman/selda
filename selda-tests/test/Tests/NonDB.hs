-- | Misc. tests that don't touch the database.
module Tests.NonDB where
import Data.List hiding (groupBy, insert)
import Data.Text (unpack)
import Database.Selda
import Database.Selda.Generic
import Test.HUnit
import Utils
import Tables

noDBTests = test
  [ "id == fromRel . toRel" ~: fromRelToRelId
  , "genTableFieldMod modifies fields" ~: gtfmModifiesFields
  ]

fromRelToRelId =
    assertEqual "fromRel . toRel /= id" genPeopleItems xs
  where
    xs = map (fromRel . toRel) genPeopleItems

gtfmModifiesFields =
  assertBool "Field names are unchanged from underlying record"
             ("genmod_" `isInfixOf` q)
  where
    q = unpack $ fst $ compile (select (gen genModPeople))
