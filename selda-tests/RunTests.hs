{-# LANGUAGE TypeOperators, OverloadedStrings, CPP #-}
module Main where
import Control.Monad
import Control.Monad.Catch
import Data.List hiding (groupBy)
import Data.Text (Text)
import System.Directory
import System.Exit
import Test.HUnit
import Test.HUnit.Text
import Database.Selda

#ifdef POSTGRES
-- To test the PostgreSQL backend, specify the connection info for the server
-- as PGConnectInfo.pgConnectInfo :: PGConnectInfo.
import Database.Selda.PostgreSQL
import PGConnectInfo (pgConnectInfo)
#else
import Database.Selda.SQLite
#endif

people :: Table (Text :*: Int :*: Maybe Text :*: Double)
people =
    table "people"
  $ primary "name"
  ¤ required "age"
  ¤ optional "pet"
  ¤ required "cash"

addresses :: Table (Text :*: Text)
addresses =
    table "addresses"
  $ required "name"
  ¤ required "city"

comments :: Table (Auto Int :*: Maybe Text :*: Text)
comments =
    table "comments"
  $ autoPrimary "id"
  ¤ optional "author"
  ¤ required "comment"

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
  createTable addresses
  createTable comments
  insert_ people peopleItems
  insert_ addresses addressItems
  insert_ comments commentItems

teardown :: SeldaT IO ()
teardown = do
  tryDropTable people
  tryDropTable addresses
  tryDropTable comments

main = do
  tmpdir <- getTemporaryDirectory
  let dbfile = tmpdir ++ "/" ++ "__selda_test_tmp.sqlite"
  freshEnv dbfile $ teardown
  result <- runTestTT (allTests dbfile)
  case result of
    Counts cs tries 0 0 -> return ()
    _                   -> exitFailure

-- | Run the given computation over the given SQLite file. If the file exists,
--   it will be removed first.
freshEnv :: FilePath -> SeldaT IO a -> IO a
#ifdef POSTGRES
freshEnv _ m = withPostgreSQL pgConnectInfo $ teardown >> m
#else
freshEnv file m = do
  exists <- doesFileExist file
  when exists $ removeFile file
  x <- withSQLite file m
  removeFile file
  return x
#endif

-- | Assert that the given computation should fail.
assertFail :: SeldaT IO a -> SeldaT IO ()
assertFail m = do
  res <- try m
  case res of
    Left (SomeException _) -> return ()
    _                      -> liftIO $ assertFailure "computation did not fail"

-- | @SeldaT@ wrapper for 'assertEqual'.
assEq :: (Show a, Eq a) => String -> a -> a -> SeldaT IO ()
assEq s expect actual = liftIO $ assertEqual s expect actual

-- | @SeldaT@ wrapper for 'assertBool'.
ass :: String -> Bool -> SeldaT IO ()
ass s pred = liftIO $ assertBool s pred

allTests f = TestList
  [ queryTests f
  , freshEnvTests f
  ]


-- Tests that don't mutate the database

queryTests f = test
  [ "setup succeeds" ~: run setup
  , "simple select" ~: run simpleSelect
  , "simple product"  ~: run simpleProduct
  , "order ascending"  ~: run orderAscending
  , "filter equal"  ~: run filterEqual
  , "filter not equal"  ~: run filterNotEqual
  , "join-like product" ~: run joinLikeProduct
  , "simple left join" ~: run simpleLeftJoin
  , "left join followed by product" ~: run leftJoinThenProduct
  , "count aggregation" ~: run countAggregate
  , "aggregate with join and group" ~: run joinGroupAggregate
  , "nested left join" ~: run nestedLeftJoin
  , "order + limit" ~: run orderLimit
  , "aggregate with doubles" ~: run aggregateWithDoubles
  , "teardown succeeds" ~: run teardown
  ]
  where
#ifdef POSTGRES
    run = withPostgreSQL pgConnectInfo
#else
    run = withSQLite f
#endif

simpleSelect = do
  ppl <- query $ select people
  assEq "wrong results from select" (sort peopleItems) (sort ppl)

simpleProduct = do
  prod <- query $ do
    name :*: city <- select addresses
    person <- select people
    return (name :*: city :*: person)
  assEq "wrong results from product" (sort ans) (sort prod)
  where
    ans = [n :*: c :*: p | p <- peopleItems, (n :*: c) <- addressItems]

orderAscending = do
  ppl <- query $ do
    name :*: rest <- select people
    order name ascending
    return (name :*: rest)
  assEq "result not properly sorted" (sort peopleItems) ppl

filterEqual = do
  ppl <- query $ do
    name :*: rest <- select people
    restrict (name .== "Link")
    return name
  assEq "unequal elements not removed" ["Link"] ppl

filterNotEqual = do
  ppl <- query $ do
    name :*: rest <- select people
    restrict (name ./= "Link")
    return name
  ass "filtered element still in list" (not $ "Link" `elem` ppl)

joinLikeProduct = do
  res <- query $ do
    name :*: rest <- select people
    name' :*: city <- select addresses
    restrict (name .== name')
    return (name :*: city)
  assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans = [n :*: c | n :*: _ <- peopleItems, n' :*: c <- addressItems, n == n']

simpleLeftJoin = do
  res <- query $ do
    name :*: rest <- select people
    _ :*: city <- leftJoin (\(name' :*: _) -> name .== name')
                           (select addresses)
    return (name :*: city)
  assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
      [ "Link"      :*: Just "Kakariko"
      , "Velvet"    :*: Nothing
      , "Miyu"      :*: Just "Fuyukishi"
      , "Kobayashi" :*: Just "Tokyo"
      ]

leftJoinThenProduct = do
  res <- query $ do
    name :*: rest <- select people
    _ :*: city <- leftJoin (\(name' :*: _) -> name .== name')
                           (select addresses)
    _ :*: name' :*: c <- select comments
    restrict (name' .== just name)
    return (name :*: city :*: c)
  assEq "join + product gave wrong result" ans res
  where
    linkComment = head [c | n :*: c <- commentItems, n == Just "Link"]
    ans = ["Link" :*: Just "Kakariko" :*: linkComment]

countAggregate = do
  [res] <- query . aggregate $ do
    _ :*: _ :*: pet :*: _ <- select people
    return (count pet)
  assEq "count counted the wrong number of pets" ans res
  where
    ans = length [pet | _ :*: _ :*: Just pet :*: _ <- peopleItems]

joinGroupAggregate = do
  res <- query . aggregate $ do
    name :*: _ :*: pet :*: _ <- select people
    _ :*: city <- leftJoin (\(name' :*: _) -> name .== name')
                           (select addresses)
    nopet <- groupBy (pet `is` null_)
    return (nopet :*: count city)
  assEq "wrong number of cities per pet owneship status" ans (sort res)
  where
    -- There are pet owners in Tokyo and Kakariko, there is no pet owner in
    -- Fuyukishi
    ans = [False :*: 2, True :*: 1]

nestedLeftJoin = do
  res <- query $ do
    name :*: _ :*: pet :*: _ <- select people
    _ :*: city :*: cs <- leftJoin (\(name' :*: _) -> name .== name') $ do
      name' :*: city <- select addresses
      _ :*: cs <- leftJoin (\(n :*: _) -> n .== just name') $ aggregate $ do
        _ :*: name' :*: comment <- select comments
        n <- groupBy name'
        return (n :*: count comment)
      return (name' :*: city :*: cs)
    return (name :*: city :*: cs)
  ass ("user with comment not in result: " ++ show res) (link `elem` res)
  ass ("user without comment not in result: " ++ show res) (velvet `elem` res)
  where
    link = "Link" :*: Just "Kakariko" :*: Just (1 :: Int)
    velvet = "Velvet" :*: Nothing :*: Nothing

orderLimit = do
  res <- query $ do
    name :*: age :*: pet :*: cash <- select people
    order cash descending
    limit 1 2
    return name
  assEq "got wrong result" ["Link", "Velvet"] (sort res)

aggregateWithDoubles = do
  [res] <- query $ aggregate $ do
    name :*: age :*: pet :*: cash <- select people
    return (avg cash)
  assEq "got wrong result" ans res
  where
    ans = sum (map fourth peopleItems)/fromIntegral (length peopleItems)


-- Tests that mutate the database

freshEnvTests f = test
  [ "tryDrop never fails"       ~: freshEnv f tryDropNeverFails
  , "tryCreate never fails"     ~: freshEnv f tryCreateNeverFails
  , "drop fails on missing"     ~: freshEnv f dropFailsOnMissing
  , "create fails on duplicate" ~: freshEnv f createFailsOnDuplicate
  , "auto primary increments"   ~: freshEnv f autoPrimaryIncrements
  ]

tryDropNeverFails = teardown
tryCreateNeverFails = tryCreateTable comments >> tryCreateTable comments
dropFailsOnMissing = assertFail $ dropTable comments
createFailsOnDuplicate = createTable people >> assertFail (createTable people)

autoPrimaryIncrements = do
  setup
  k <- insertWithPK comments [Just "Kobayashi" :*: "チョロゴン" ]
  k' <- insertWithPK comments [Nothing :*: "more anonymous spam" ]
  [name] <- query $ do
    id :*: name :*: _ <- select comments
    restrict (id .== int k)
    return name
  assEq "inserted key refers to wrong value" name (Just "Kobayashi")
  ass "primary key doesn't increment properly" (k' == k+1)
