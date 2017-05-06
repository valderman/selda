{-# LANGUAGE TypeOperators, OverloadedStrings #-}
-- | Tests that don't modify the database.
module Tests.Query (queryTests) where
import Data.List hiding (groupBy, insert)
import Database.Selda
import Database.Selda.Generic
import Database.Selda.Unsafe
import Test.HUnit
import Utils
import Tables

queryTests run = test
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
  , "limit gives correct number of results" ~: run limitCorrectNumber
  , "aggregate with doubles" ~: run aggregateWithDoubles
  , "generic query on ad hoc table" ~: run genQueryAdHocTable
  , "generic query on generic table" ~: run genQueryGenTable
  , "ad hoc query on generic table" ~: run adHocQueryGenTable
  , "select from value table" ~: run selectVals
  , "select from empty value table" ~: run selectEmptyValues
  , "aggregate from empty value table" ~: run aggregateEmptyValues
  , "inner join" ~: run testInnerJoin
  , "rounding doubles to ints" ~: run roundToInt
  , "serializing doubles" ~: run serializeDouble
  , "teardown succeeds" ~: run teardown
  ]

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

joinLikeProductWithSels = do
  res <- query $ do
    p <- select people
    a <- select addresses
    restrict (p ! pName .== a ! aName)
    return (p ! pName :*: a ! aCity :*: p ! pPet)
  assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
      [ n :*: c :*: p
      | n :*: _ :*: p :*: _ <- peopleItems
      , n' :*: c <- addressItems
      , n == n'
      ]

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
    nopet <- groupBy (isNull pet)
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
  res <- query $ limit 1 2 $ do
    name :*: age :*: pet :*: cash <- select people
    order cash descending
    return name
  assEq "got wrong result" ["Link", "Velvet"] (sort res)

limitCorrectNumber = do
  res <- query $ do
    p1 <- limit 1 2 $ select people
    p2 <- limit 1 2 $ select people
    return p1
  assEq ("wrong number of results from limit") 4 (length res)

aggregateWithDoubles = do
  [res] <- query $ aggregate $ do
    name :*: age :*: pet :*: cash <- select people
    return (avg cash)
  assEq "got wrong result" ans res
  where
    ans = sum (map fourth peopleItems)/fromIntegral (length peopleItems)

genQueryAdHocTable = do
  ppl <- map fromRel <$> query (select people)
  assEq "wrong results from fromRel" (sort genPeopleItems) (sort ppl)

genQueryGenTable = do
    ppl1 <- query $ do
      person <- select $ gen genPeople
      restrict (person ! pCash .> 0)
      return (person ! pName :*: person ! pAge)
    assEq "query gave wrong result" (sort ppl2) (sort ppl1)
  where
    ppl2 = [name p :*: age p | p <- genPeopleItems, cash p > 0]

adHocQueryGenTable = do
    ppl1 <- query $ do
      name :*: age :*: pet :*: cash <- select $ gen genPeople
      restrict (cash .> 0)
      return (name :*: age)
    assEq "query gave wrong result" (sort ppl2) (sort ppl1)
  where
    ppl2 = [name p :*: age p | p <- genPeopleItems, cash p > 0]

selectVals = do
  vals <- query $ selectValues peopleItems
  assEq "wrong columns returned" (sort peopleItems) (sort vals)

selectEmptyValues = do
  res <- query $ do
    ppl <- select people
    vals <- selectValues ([] :: [Maybe Text])
    cs <- select comments
    return cs
  assEq "result set wasn't empty" [] res

aggregateEmptyValues = do
  [res] <- query $ aggregate $ do
    ppl <- select people
    vals <- selectValues ([] :: [Int :*: Int])
    id :*: _ <- select comments
    return (count id)
  assEq "wrong count for empty result set" 0 res

testInnerJoin = do
    res <- query $ do
      p <- select people
      a <- innerJoin (\a -> p ! pName .== a ! aName) $ do
        select addresses
      return (p ! pPet :*: a ! aCity)
    assEq "wrong result" oracle res
  where
    oracle =
      [ Just "horse"  :*: "Kakariko"
      , Just "dragon" :*: "Tokyo"
      , Nothing       :*: "Fuyukishi"
      ]

roundToInt = do
  res <- query $ round_ <$> selectValues [1.1, 1.5, 1.9 :: Double]
  assEq "bad rounding" [1, 2, 2 :: Int] res

serializeDouble = do
  -- The "protocol" used by PostgreSQL is insane - better check that we speak
  -- it properly!
  res <- query $ do
    n <- selectValues [123456789 :: Int]
    d <- selectValues [123456789.3 :: Double]
    restrict (d .> cast n)
    return (cast n + float 1.123)
  assEq "wrong encoding" 1 (length res)
  assEq "wrong decoding" [123456790.123] res
