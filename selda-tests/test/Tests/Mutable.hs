{-# LANGUAGE TypeOperators, OverloadedStrings #-}
-- | Tests that modify the database.
module Tests.Mutable (mutableTests, invalidateCacheAfterTransaction) where
import Control.Concurrent
import Control.Monad.Catch
import Data.List hiding (groupBy, insert)
import Data.Time
import Database.Selda
import Database.Selda.Backend
import Database.Selda.Generic
import Test.HUnit
import Utils
import Tables

mutableTests freshEnv = test
  [ "tryDrop never fails"            ~: freshEnv tryDropNeverFails
  , "tryCreate never fails"          ~: freshEnv tryCreateNeverFails
  , "drop fails on missing"          ~: freshEnv dropFailsOnMissing
  , "create fails on duplicate"      ~: freshEnv createFailsOnDuplicate
  , "auto primary increments"        ~: freshEnv autoPrimaryIncrements
  , "insert returns number of rows"  ~: freshEnv insertReturnsNumRows
  , "update updates table"           ~: freshEnv updateUpdates
  , "update nothing"                 ~: freshEnv updateNothing
  , "insert time values"             ~: freshEnv insertTime
  , "transaction completes"          ~: freshEnv transactionCompletes
  , "transaction rolls back"         ~: freshEnv transactionRollsBack
  , "queries are consistent"         ~: freshEnv consistentQueries
  , "delete deletes"                 ~: freshEnv deleteDeletes
  , "generic delete"                 ~: freshEnv genericDelete
  , "generic update"                 ~: freshEnv genericUpdate
  , "generic insert"                 ~: freshEnv genericInsert
  , "ad hoc insert in generic table" ~: freshEnv adHocInsertInGenericTable
  , "delete everything"              ~: freshEnv deleteEverything
  , "override auto-increment"        ~: freshEnv overrideAutoIncrement
  , "insert all defaults"            ~: freshEnv insertAllDefaults
  , "insert some defaults"           ~: freshEnv insertSomeDefaults
  , "quoted weird names"             ~: freshEnv weirdNames
  , "dupe insert throws SeldaError"  ~: freshEnv dupeInsertThrowsSeldaError
  , "dupe insert 2 throws SeldaError"~: freshEnv dupeInsert2ThrowsSeldaError
  , "dupe update throws SeldaError"  ~: freshEnv dupeUpdateThrowsSeldaError
  , "nul queries don't fail"         ~: freshEnv nulQueries
  , "fk violation fails"             ~: freshEnv fkViolationFails
  , "table with multiple FKs"        ~: freshEnv multipleFKs
  , "uniqueness violation fails"     ~: freshEnv uniqueViolation
  , "upsert inserts/updates right"   ~: freshEnv insertOrUpdate
  , "tryInsert doesn't fail"         ~: freshEnv tryInsertDoesntFail
  , "isIn [...] gives right result"  ~: freshEnv isInList
  ]

tryDropNeverFails = teardown
tryCreateNeverFails = tryCreateTable comments >> tryCreateTable comments
dropFailsOnMissing = assertFail $ dropTable comments
createFailsOnDuplicate = createTable people >> assertFail (createTable people)

autoPrimaryIncrements = do
  setup
  k <- insertWithPK comments [def :*: Just "Kobayashi" :*: "チョロゴン" ]
  k' <- insertWithPK comments [def :*: Nothing :*: "more anonymous spam"]
  [name] <- query $ do
    id :*: name :*: _ <- select comments
    restrict (id .== int k)
    return name
  assEq "inserted key refers to wrong value" name (Just "Kobayashi")
  ass "primary key doesn't increment properly" (k' == k+1)

insertReturnsNumRows = do
  setup
  rows <- insert comments
    [ def :*: Just "Kobayashi" :*: "チョロゴン"
    , def :*: Nothing :*: "more anonymous spam"
    , def :*: Nothing :*: "even more spam"
    ]
  assEq "insert returns wrong number of inserted rows" 3 rows

updateUpdates = do
  setup
  insert_ comments
    [ def :*: Just "Kobayashi" :*: "チョロゴン"
    , def :*: Nothing :*: "more anonymous spam"
    , def :*: Nothing :*: "even more spam"
    ]
  rows <- update comments (isNull . second)
                          (\(id :*: _ :*: c) -> (id :*: just "anon" :*: c))
  [upd] <- query $ aggregate $ do
    _ :*: name :*: _ <- select comments
    restrict (not_ $ isNull name)
    restrict (name .== just "anon")
    return (count name)
  assEq "update returns wrong number of updated rows" 3 rows
  assEq "rows were not updated" 3 upd

updateNothing = do
  setup
  a <- query $ select people
  n <- update people (const true) id
  b <- query $ select people
  assEq "identity update didn't happen" (length a) n
  assEq "identity update did something weird" a b

insertTime = do
  createTable times
  let Just t = parseTimeM True defaultTimeLocale sqlDateTimeFormat "2011-11-11 11:11:11.11111"
      Just d = parseTimeM True defaultTimeLocale sqlDateFormat "2011-11-11"
      Just lt = parseTimeM True defaultTimeLocale sqlTimeFormat "11:11:11.11111"
  insert_ times ["now" :*: t :*: d :*: lt]
  ["now" :*: t' :*: d' :*: lt'] <- query $ select times
  assEq "time not properly inserted" (t, d, lt) (t', d', lt')
  dropTable times
  where
    times :: Table (Text :*: UTCTime :*: Day :*: TimeOfDay)
    times =
          table "times"
      $   required "description"
      :*: required "time"
      :*: required "day"
      :*: required "local_tod"

transactionCompletes = do
  setup
  transaction $ do
    insert_ comments [def :*: Just "Kobayashi" :*: c1]
    insert_ comments
      [ def :*: Nothing :*: "more anonymous spam"
      , def :*: Just "Kobayashi" :*: c2
      ]
  cs <- query $ do
    _ :*: name :*: comment <- select comments
    restrict (name .== just "Kobayashi")
    return comment
  ass "some inserts were not performed"
      (c1 `elem` cs && c2 `elem` cs && length cs == 2)
  where
    c1 = "チョロゴン"
    c2 = "メイド最高！"

transactionRollsBack = do
  setup
  res <- try $ transaction $ do
    insert_ comments [def :*: Just "Kobayashi" :*: c1]
    insert_ comments
      [ def :*: Nothing :*: "more anonymous spam"
      , def :*: Just "Kobayashi" :*: c2
      ]
    fail "nope"
  case res of
    Right _ ->
      liftIO $ assertFailure "exception didn't propagate"
    Left (SomeException _) -> do
      cs <- query $ do
        _ :*: name :*: comment <- select comments
        restrict (name .== just "Kobayashi")
        return comment
      assEq "commit was not rolled back" [] cs
  where
    c1 = "チョロゴン"
    c2 = "メイド最高！"

consistentQueries = do
  setup
  a <- query q
  b <- query q
  assEq "query result changed on its own" a b
  where
    q = do
      (name :*: age :*: _ :*: cash) <- select people
      restrict (round_ cash .> age)
      return name

deleteDeletes = do
  setup
  a <- query q
  deleteFrom_ people (\(name :*: _) -> name .== "Velvet")
  b <- query q
  ass "rows not deleted" (a /= b && length b < length a)
  where
    q = do
      (name :*: age :*: _ :*: cash) <- select people
      restrict (round_ cash .< age)
      return name

deleteEverything = do
  tryDropTable people
  createTable people
  insert_ people peopleItems
  a <- query q
  deleteFrom_ people (const true)
  b <- query q
  ass "table empty before delete" (a /= [])
  assEq "rows not deleted" [] b
  where
    q = do
      (name :*: age :*: _ :*: cash) <- select people
      restrict (round_ cash .> age)
      return name

genericDelete = do
  setup
  deleteFrom_ (gen genPeople) (\p -> p ! pCash .> 0)
  monies <- query $ do
    p <- select (gen genPeople)
    return (p ! pCash)
  ass "deleted wrong items" $ all (<= 0) monies

genericUpdate = do
  setup
  update_ (gen genPeople) (\p -> p ! pCash .> 0)
                          (\p -> p `with` [pCash := 0])
  monies <- query $ do
    p <- select (gen genPeople)
    return (p ! pCash)
  ass "update failed" $ all (<= 0) monies

genericInsert = do
  setup
  q1 <- query $ select (gen genPeople)
  deleteFrom_ (gen genPeople) (const true)
  insertGen_ genPeople genPeopleItems
  q2 <- query $ select (gen genPeople)
  assEq "insert failed" (sort q1) (sort q2)

adHocInsertInGenericTable = do
  setup
  insert_ (gen genPeople) [val]
  [val'] <- query $ do
    p <- select (gen genPeople)
    restrict (p ! pName .== "Saber")
    return p
  assEq "insert failed" val val'
  where
    val = "Saber" :*: 1537 :*: Nothing :*: 0

overrideAutoIncrement = do
  setup
  insert_ comments [123 :*: Nothing :*: "hello"]
  num <- query $ aggregate $ do
    id :*: _ <- select comments
    restrict (id .== 123)
    return (count id)
  assEq "failed to override auto-incrementing column" [1] num

insertAllDefaults = do
  setup
  pk <- insertWithPK comments [def :*: def :*: def]
  res <- query $ do
    comment@(id :*: _) <- select comments
    restrict (id .== int pk)
    return comment
  assEq "wrong default values inserted" [pk :*: Nothing :*: ""] res

insertSomeDefaults = do
  setup
  insert_ people ["Celes" :*: def :*: Just "chocobo" :*: def]
  res <- query $ do
    person@(id :*: n :*: pet :*: c) <- select people
    restrict (pet .== just "chocobo")
    return person
  assEq "wrong values inserted" ["Celes" :*: 0 :*: Just "chocobo" :*: 0] res

weirdNames = do
  tryDropTable tableWithWeirdNames
  createTable tableWithWeirdNames
  i1 <- insert tableWithWeirdNames [42 :*: Nothing]
  assEq "first insert failed" 1 i1
  i2 <- insert tableWithWeirdNames [123 :*: Just 321]
  assEq "second insert failed" 1 i2
  up <- update tableWithWeirdNames (\c -> c ! weird1 .== 42)
                                   (\c -> c `with` [weird2 := just 11])
  assEq "update failed" 1 up
  res <- query $ do
    t <- select tableWithWeirdNames
    restrict (t ! weird1 .== 42)
    return (t ! weird2)
  assEq "select failed" [Just 11] res
  dropTable tableWithWeirdNames
  where
    tableWithWeirdNames :: Table (Int :*: Maybe Int)
    (tableWithWeirdNames, weird1 :*: weird2) =
          tableWithSelectors "DROP TABLE comments"
      $   required "one \" quote \1\2\3\DEL"
      :*: optional "two \"quotes\""

dupeInsertThrowsSeldaError = do
  setup
  assertFail $ do
    insert_ comments
      [ 0 :*: Just "Kobayashi" :*: "チョロゴン"
      , 0 :*: Nothing          :*: "some spam"
      ]

dupeInsert2ThrowsSeldaError = do
  setup
  insert_ comments [0 :*: Just "Kobayashi" :*: "チョロゴン"]
  e <- try $ insert_ comments [0 :*: Nothing :*: "Spam, spam, spaaaaaam!"]
  case e :: Either SeldaError () of
    Left _ -> return ()
    _      -> liftIO $ assertFailure "SeldaError not thrown"

dupeUpdateThrowsSeldaError = do
  setup
  insert_ comments
    [ 0   :*: Just "Kobayashi" :*: "チョロゴン"
    , def :*: Just "spammer"   :*: "some spam"
    ]
  e <- try $ do
    update_ comments
      (\c -> c ! cName .== just "spammer")
      (\c -> c `with` [cId := 0])
  case e :: Either SeldaError () of
    Left _ -> return ()
    _      -> liftIO $ assertFailure "SeldaError not thrown"

nulQueries = do
  setup
  insert_ comments
    [ def :*: Just "Kobayashi" :*: "チョロゴン"
    , def :*: Nothing          :*: "more \0 spam"
    , def :*: Nothing          :*: "even more spam"
    ]
  rows <- update comments (isNull . second)
                          (\(id :*: _ :*: c) -> (id :*: just "\0" :*: c))
  [upd] <- query $ aggregate $ do
    _ :*: name :*: _ <- select comments
    restrict (not_ $ isNull name)
    restrict (name .== just "\0")
    return (count name)
  assEq "update returns wrong number of updated rows" 3 rows
  assEq "rows were not updated" 3 upd

invalidateCacheAfterTransaction run = run $ do
  setLocalCache 1000
  tryDropTable comments
  tryDropTable addresses
  createTable comments
  createTable addresses
  lock <- liftIO $ newEmptyMVar

  -- This thread repopulates the cache for the query before the transaction
  -- in which it was invalidated finishes
  liftIO $ forkIO $ run $ do
    liftIO $ takeMVar lock
    query $ do
      c <- select comments
      restrict (c ! cName .== just "Link")
      return (c ! cComment)
    liftIO $ putMVar lock ()

  insert_ comments [def :*: Just "Link" :*: "spam"]
  transaction $ do
    update_ comments
      (\c -> c ! cName .== just "Link")
      (\c -> c `with` [cComment := "insightful comment"])
    liftIO $ putMVar lock ()
    liftIO $ takeMVar lock
    insert_ addresses [def :*: def]

  -- At this point, the comment in the database is "insightful comment", but
  -- unless the cache is re-invalidated *after* the transaction finishes,
  -- the cached comment will be "spam".
  [comment] <- query $ do
    c <- select comments
    restrict (c ! cName .== just "Link")
    return (c ! cComment)
  assEq "" "insightful comment" comment

fkViolationFails = do
    -- Note that this is intended to test that FKs are in place and enabled.
    -- If we get an FK violation here, we assume that the database does the
    -- right thing in other situations, since FKs behavior is determined by
    -- the DB, not by Selda, except when creating tables.
    setup
    createTable addressesWithFK
    assertFail $ insert_ addressesWithFK ["Nobody" :*: "Nowhere"]
    dropTable addressesWithFK
  where
    addressesWithFK :: Table (Text :*: Text)
    addressesWithFK =
          table "addressesWithFK"
      $   required "name" `fk` (people, pName)
      :*: required "city"

multipleFKs = do
    setup
    createTable addressesWithFK
    assertFail $ insert_ addressesWithFK ["Nobody" :*: "Nowhere"]
    dropTable addressesWithFK
  where
    addressesWithFK :: Table (Text :*: Text)
    addressesWithFK =
          table "addressesWithFK"
      $   required "name" `fk` (people, pName) `fk` (people, pName)
      :*: required "city"

uniqueViolation = do
    createTable uniquePeople
    assertFail $ insert_ uniquePeople
      [ "Link" :*: Nothing
      , "Link" :*: Nothing
      ]
    r1 <- query $ select uniquePeople
    assertFail $ do
      insert_ uniquePeople ["Link" :*: Nothing]
      insert_ uniquePeople ["Link" :*: Nothing]
    r2 <- query $ select uniquePeople
    assEq "inserted rows despite constraint violation" [] r1
    assEq "row disappeared after violation" ["Link" :*: Nothing] r2
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text :*: Maybe Text)
    (uniquePeople, upName :*: upPet) =
          tableWithSelectors "uniquePeople"
      $   unique (required "name")
      :*: optional "pet"

insertOrUpdate = do
    createTable counters
    upsert counters
           (\(c :*: v) -> c .== 0)
           (\(c :*: v) -> c :*: v+1)
           [0 :*: 1]
    upsert counters
           (\(c :*: v) -> c .== 0)
           (\(c :*: v) -> c :*: v+1)
           [0 :*: 1]
    res <- query $ select counters
    assEq "wrong value for counter" [0 :*: 2] res
    dropTable counters
  where
    counters :: Table (Int :*: Int)
    counters =
          table "counters"
      $   primary "id"
      :*: required "count"

tryInsertDoesntFail = do
    createTable uniquePeople
    res1 <- tryInsert uniquePeople ["Link" :*: Nothing]
    r1 <- query $ select uniquePeople
    res2 <- tryInsert uniquePeople ["Link" :*: Nothing]
    r2 <- query $ select uniquePeople
    assEq "wrong return value from successful tryInsert" True res1
    assEq "row not inserted" ["Link" :*: Nothing] r1
    assEq "wrong return value from failed tryInsert" False res2
    assEq "row inserted despite violation" ["Link" :*: Nothing] r2
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text :*: Maybe Text)
    (uniquePeople, upName :*: upPet) =
          tableWithSelectors "uniquePeople"
      $   unique (required "name")
      :*: optional "pet"

isInList = do
  setup
  res <- query $ do
    p <- select people
    restrict (p ! pName .== "Link")
    return (  "Link" `isIn` [p ! pName, "blah"]
           :*: 0 `isIn` [p ! pAge, 42, 19]
           :*: 1 `isIn` ([] :: [Col () Int])
           )
  assEq "wrong result from isIn" [True :*: False :*: False] res
