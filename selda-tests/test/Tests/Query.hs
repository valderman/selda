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
  , "simple if-then-else" ~: run simpleIfThenElse
  , "rounding doubles to ints" ~: run roundToInt
  , "serializing doubles" ~: run serializeDouble
  , "such that works" ~: run testSuchThat
  , "prepared without args" ~: run preparedNoArgs
  , "prepared with args" ~: run preparedManyArgs
  , "prepared interleaved" ~: run preparedInterleaved
  , "interleaved with different results" ~: run preparedDifferentResults
  , "order in correct order" ~: run orderCorrectOrder
  , "multiple aggregates in sequence (#42)" ~: run multipleAggregates
  , "isIn inner query renaming (#46)" ~: run isInQueryRenaming
  , "distinct on multiple queries" ~: run selectDistinct
  , "distinct on single query" ~: run selectValuesDistinct
  , "matchNull" ~: run simpleMatchNull
  , "ifThenElse" ~: run simpleIfThenElse
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

simpleIfThenElse = do
    ppl <- query $ do
      name :*: age :*: _ :*: _ <- select people
      let ageGroup = ifThenElse (age .< 18)  (text "Child") $
                     ifThenElse (age .>= 65) (text "Elder")
                                             (text "Adult")
      return (name :*: age :*: ageGroup)
    assEq "wrong results from ifThenElse" (sort res) (sort ppl)
  where
    res =
      [ "Link"      :*: 125 :*: "Elder"
      , "Velvet"    :*: 19  :*: "Adult"
      , "Kobayashi" :*: 23  :*: "Adult"
      , "Miyu"      :*: 10  :*: "Child"
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

testSuchThat = do
  res <- query $ do
    n1 <- (pName `from` select people) `suchThat` (.== "Link")
    n2 <- (pName `from` select people) `suchThat` (.== "Velvet")
    return (n1 :*: n2)
  assEq "got wrong result" ["Link" :*: "Velvet"] res

{-# NOINLINE allShortNames #-}
allShortNames :: SeldaM [Text]
allShortNames = prepared $ do
  p <- select people
  restrict (length_ (p ! pName) .<= 4)
  order (p ! pName) ascending
  return (p ! pName)

preparedNoArgs = do
    res1 <- allShortNames
    res2 <- allShortNames
    res3 <- allShortNames
    assEq "got wrong result" res res1
    ass "subsequent calls gave different results" (all (== res1) [res2, res3])
  where
    res = ["Link", "Miyu"]

{-# NOINLINE allNamesLike #-}
-- Extra restricts to force the presence of a few non-argument parameters.
allNamesLike :: Int -> Text -> SeldaM [Text]
allNamesLike = prepared $ \len s -> do
  p <- select people
  restrict (length_ (p ! pName) .> 0)
  restrict (p ! pName `like` s)
  restrict (length_ (p ! pName) .> 1)
  restrict (length_ (p ! pName) .<= len)
  restrict (length_ (p ! pName) .<= 100)
  restrict (length_ (p ! pName) .<= 200)
  order (p ! pName) ascending
  return (p ! pName)

preparedManyArgs = do
    res1 <- allNamesLike 4 "%y%"
    res2 <- allNamesLike 5 "%y%"
    res3 <- allNamesLike 6 "%y%"
    assEq "got wrong result" res res1
    ass "subsequent calls gave different results" (all (== res1) [res2, res3])
  where
    res = ["Miyu"]

preparedInterleaved = do
    asn1 <- allShortNames
    anl1 <- allNamesLike 4 "%y%"
    asn2 <- allShortNames
    anl2 <- allNamesLike 5 "%y%"
    asn3 <- allShortNames
    anl3 <- allNamesLike 6 "%y%"
    assEq "wrong result from allShortNames" asnres asn1
    assEq "wrong result from allNamesLike" anlres anl1
    ass "subsequent allShortNames calls gave different results"
        (all (==asn1) [asn2, asn3])
    ass "subsequent allNamesLike calls gave different results"
        (all (==anl1) [anl2, anl3])
  where
    asnres = ["Link", "Miyu"]
    anlres = ["Miyu"]

preparedDifferentResults = do
  res1 <- allNamesLike 4 "%y%"
  res2 <- allNamesLike 10 "%y%"
  assEq "wrong result from first query" ["Miyu"] res1
  assEq "wrong result from second query" ["Kobayashi", "Miyu"] res2

orderCorrectOrder = do
    insert_ people ["Amber" :*: 19 :*: Nothing :*: 123]

    res1 <- query $ do
      p <- select people
      order (p ! pName) ascending
      order (p ! pAge) ascending
      return (p ! pName)

    res2 <- query $ do
      p <- select people
      order (p ! pName) descending
      order (p ! pAge) ascending
      return (p ! pName)

    res3 <- query $ do
      p <- select people
      order (p ! pAge) descending
      order (p ! pName) ascending
      return (p ! pName)

    deleteFrom_ people $ \p -> p ! pName .== "Amber"

    assEq "latest ordering did not take precedence in first query" ans1 res1
    assEq "latest ordering did not take precedence in second query" ans2 res2
    assEq "latest ordering did not take precedence in third query" ans3 res3
  where
    ans1 = ["Miyu", "Amber", "Velvet", "Kobayashi", "Link"]
    ans2 = ["Miyu", "Velvet", "Amber", "Kobayashi", "Link"]
    ans3 = ["Amber", "Kobayashi", "Link", "Miyu", "Velvet"]

-- Test case for #42: name supply was erroneously overwritten when using
-- aggregates.
multipleAggregates = do
  res <- query $ do
    (name :*: _ :*: _) <- select people

    (owner :*: homes) <- aggregate $ do
      (owner :*: city) <- select addresses
      owner' <- groupBy owner
      return (owner' :*: count city)
    restrict (owner .== name)

    (owner2 :*: homesInTokyo) <- aggregate $ do
      (owner :*: city) <- select addresses
      restrict (city .== "Tokyo")
      owner' <- groupBy owner
      return (owner' :*: count city)
    restrict (owner2 .== name)

    order homes descending
    return (owner :*: homes :*: homesInTokyo)
  assEq "wrong result for aggregate query" ["Kobayashi" :*: 1 :*: 1] res

isInQueryRenaming = do
  res <- query $ do
    (name :*: _ :*: _) <- select people
    restrict $ (int 1) `isIn` (do
        (name2 :*: age :*: _) <- select people
        (name3 :*: city) <- select addresses
        restrict (name3 .== name2)
        restrict (name .== name2)
        restrict (city .== "Kakariko")
        return (int 1)
      )
    return name
  assEq "wrong list of people returned" ["Link"] res

selectDistinct = do
  res <- query $ distinct $ do
    (name :*: _ :*: _) <- select people
    select people
    order name ascending
    return name
  assEq "wrong result set" ["Kobayashi", "Link", "Miyu", "Velvet"] res

selectValuesDistinct = do
  res <- query $ distinct $ selectValues $ replicate 5 ("Link" :: Text)
  assEq "wrong result set" ["Link"] res

simpleMatchNull = do
    res <- query $ do
      (name :*: _ :*: pet :*: _) <- select people
      order name ascending
      return $ (name :*: matchNull 0 length_ pet)
    assEq "wrong result set" expected res
  where
    expected =
      [ "Kobayashi" :*: 6
      , "Link" :*: 5
      , "Miyu" :*: 0
      , "Velvet" :*: 0
      ]
