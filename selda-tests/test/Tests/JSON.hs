{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}
module Tests.JSON (jsonTests, jsonQueryTests) where
import Database.Selda hiding (Result)
import Database.Selda.JSON
import Database.Selda.Nullable (nonNull)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.List (sort)
import Test.HUnit
import Tables (Person (..), peopleItems)
import Utils

data JSONPerson = JSONPerson
  { id :: ID JSONPerson
  , nameKey :: Text
  , json :: Value
  } deriving Generic
instance SqlRow JSONPerson

instance ToJSON Person
instance FromJSON Person

flipWithM_ :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m ()
flipWithM_ xs ys f = zipWithM_ f xs ys

jsonPeople :: Table JSONPerson
jsonPeople = table "jsonPeople" [#id :- autoPrimary]

jsonTests :: (SeldaM b () -> IO ()) -> Test
jsonTests freshEnv = test
  [ "json is properly inserted"    ~: freshEnv insertJson
  , "json is properly returned"    ~: freshEnv selectJson
  ]

jsonQueryTests :: JSONBackend b => (SeldaM b () -> IO ()) -> Test
jsonQueryTests freshEnv = test
  [ "can select json properties"   ~: freshEnv selectJsonProp
  , "select dynamic json property" ~: freshEnv selectJsonPropDynamic
  , "convert json to string"       ~: freshEnv json2String
  , "missing property yields null" ~: freshEnv missingProp
  , "chained lookup"               ~: freshEnv chainedLookup
  ]


numPeopleItems :: Int
numPeopleItems = length peopleItems

withJsonTable :: (Int -> SeldaM b a) -> SeldaM b a
withJsonTable go = do
  tryDropTable jsonPeople
  createTable jsonPeople
  count <- insert jsonPeople $ map (JSONPerson def "name" . toJSON) peopleItems
  x <- go count
  dropTable jsonPeople
  return x

withJsonTable' :: SeldaM b a -> SeldaM b a
withJsonTable' go = withJsonTable (const go)

insertJson =
  withJsonTable $ assEq "wrong number of rows inserted" numPeopleItems

selectJson = withJsonTable' $ do
  vals <- query $ #json `from` select jsonPeople
  let vals' = [v | Success v <- map fromJSON vals]
  assEq "wrong number of rows returned" numPeopleItems (length vals)
  assEq "some json conversions failed" (length vals) (length vals')
  flipWithM_ (sort peopleItems) (sort vals') $ \expected actual -> do
    assEq "got wrong element from query" expected actual

selectJsonProp :: JSONBackend b => SeldaM b ()
selectJsonProp = withJsonTable' $ do
  vals <- query $ do
    json_person <- #json `from` select jsonPeople
    return $ json_person ~> "name"
  let vals' = [s | Just (String s) <- vals]
  assEq "wrong number of rows returned" numPeopleItems (length vals)
  assEq "some json conversions failed" (length vals) (length vals')
  assEq "wrong list of names returned" (sort $ map name peopleItems) (sort vals')

selectJsonPropDynamic :: JSONBackend b => SeldaM b ()
selectJsonPropDynamic = withJsonTable' $ do
  vals <- query $ do
    person <- select jsonPeople
    return $ person ! #json ~> person ! #nameKey
  let vals' = [s | Just (String s) <- vals]
  assEq "wrong number of rows returned" numPeopleItems (length vals)
  assEq "some json conversions failed" (length vals) (length vals')
  assEq "wrong list of names returned" (sort $ map name peopleItems) (sort vals')

json2String :: JSONBackend b => SeldaM b ()
json2String = withJsonTable' $ do
  vals <- query $ do
    json_person <- #json `from` select jsonPeople
    return $ jsonToText json_person
  let vals' = [x | Just x <- map (decode . BSL.fromStrict . encodeUtf8) vals]
  assEq "wrong number of rows returned" numPeopleItems (length vals)
  assEq "some json conversions failed" (length vals) (length vals')
  assEq "wrong list of people returned" (sort peopleItems) (sort vals')

missingProp :: JSONBackend b => SeldaM b ()
missingProp = withJsonTable' $ do
  vals <- query $ do
    json_person <- #json `from` select jsonPeople
    return $ json_person ~> "this property does not exist"
  assEq "wrong number of rows returned" numPeopleItems (length vals)
  forM_ vals $ \actual -> do
    assEq "some rows were not null" Nothing actual

chainedLookup :: JSONBackend b => SeldaM b ()
chainedLookup = withJsonTable' $ do
    update jsonPeople (const true) (`with` [#json := literal newJson])
    val <- fmap head . query $ do
      json_person <- #json `from` select jsonPeople
      x <- nonNull (json_person ~> "foo" ~> "bar")
      return (jsonToText x)
    assEq "wrong value returned" "42" val
  where
    Just newJson = decode "{\"foo\": {\"bar\": 42}}" :: Maybe Value
