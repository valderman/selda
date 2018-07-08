-- | Misc. tests that don't touch the database.
module Tests.NonDB where
import Data.List hiding (groupBy, insert)
import Data.Text (unpack)
import Database.Selda
import Test.HUnit
import Utils
import Tables

noDBTests = test
  [ "id == fromRel . toRel" ~: fromRelToRelId
  , "tableFieldMod modifies fields" ~: tfmModifiesFields
  ]

fromRelToRelId = do
    assertEqual "toRel . fromRel /= id" peopleItems xs
    assertEqual "fromRel . toRel /= id" people xs'
  where
    people = fromRels peopleItems :: [Person]
    xs = toRels people :: [Relation Person]
    xs' = fromRels xs :: [Person]

tfmModifiesFields =
  assertBool "Field names are unchanged from underlying record"
             ("mod_" `isInfixOf` q)
  where
    q = unpack $ fst $ compile (select modPeople)
