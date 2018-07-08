{-# LANGUAGE TypeOperators, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
-- | Tests that modify the database.
module Tests.Mutable (mutableTests, invalidateCacheAfterTransaction) where
import Control.Concurrent
import Control.Monad.Catch
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.List hiding (groupBy, insert)
import Data.Proxy
import Data.Time
import Database.Selda
import Database.Selda.Backend hiding (disableForeignKeys)
import Database.Selda.Migrations
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
  , "isIn list gives right result"   ~: freshEnv isInList
  , "isIn query gives right result"  ~: freshEnv isInQuery
  , "strict blob column"             ~: freshEnv blobColumn
  , "lazy blob column"               ~: freshEnv lazyBlobColumn
  , "insertWhen/Unless"              ~: freshEnv whenUnless
  , "insert >999 parameters"         ~: freshEnv manyParameters
  , "empty insertion"                ~: freshEnv emptyInsert
  , "correct boolean representation" ~: freshEnv boolTable
  , "optional foreign keys"          ~: freshEnv optionalFK
  , "custom enum type"               ~: freshEnv customEnum
  , "table from tuple"               ~: freshEnv genericTupleTable
  , "disable foreign key checks"     ~: freshEnv disableForeignKeys
  , "mod fk violation fails"         ~: freshEnv genModFkViolationFails
  , "mod fk insertion ok"            ~: freshEnv genModFkInsertSucceeds
  , "migrate into self"              ~: freshEnv (migrationTest migrateIntoSelf)
  , "drop column migration"          ~: freshEnv (migrationTest dropColumn)
  , "auto-migrate one step"          ~: freshEnv (migrationTest autoMigrateOneStep)
  , "auto-migrate no-op"             ~: freshEnv (migrationTest autoMigrateNoOp)
  , "migrate aggregate"              ~: freshEnv (migrationTest migrateAggregate)
  , "auto-migrate multi-step"        ~: freshEnv (migrationTest autoMigrateOneStep)
  ]

tryDropNeverFails = teardown
tryCreateNeverFails = tryCreateTable comments >> tryCreateTable comments
dropFailsOnMissing = assertFail $ dropTable comments
createFailsOnDuplicate = createTable people >> assertFail (createTable people)

autoPrimaryIncrements = do
  setup
  k <- insertWithPK comments [(def, Just "Kobayashi", "チョロゴン")]
  k' <- insertWithPK comments [(def, Nothing, "more anonymous spam")]
  [name] <- query $ do
    id :*: name :*: _ <- select comments
    restrict (id .== literal k)
    return name
  assEq "inserted key refers to wrong value" name (Just "Kobayashi")
  let k0 = read (show k) :: Int
      k1 = read (show k') :: Int
  ass "primary key doesn't increment properly" (k1 == k0+1)

insertReturnsNumRows = do
  setup
  rows <- insert comments
    [ (def, Just "Kobayashi", "チョロゴン")
    , (def, Nothing, "more anonymous spam")
    , (def, Nothing, "even more spam")
    ]
  assEq "insert returns wrong number of inserted rows" 3 rows

updateUpdates = do
  setup
  insert_ comments
    [ (def, Just "Kobayashi", "チョロゴン")
    , (def, Nothing, "more anonymous spam")
    , (def, Nothing, "even more spam")
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
  tryDropTable times
  createTable times
  let Just t = parseTimeM True defaultTimeLocale sqlDateTimeFormat "2011-11-11 11:11:11.11111"
      Just d = parseTimeM True defaultTimeLocale sqlDateFormat "2011-11-11"
      Just lt = parseTimeM True defaultTimeLocale sqlTimeFormat "11:11:11.11111"
  insert_ times [("now", t, d, lt)]
  ["now" :*: t' :*: d' :*: lt'] <- query $ select times
  assEq "time not properly inserted" (t, d, lt) (t', d', lt')
  dropTable times
  where
    times :: Table (Text, UTCTime, Day, TimeOfDay)
    times = table "times" []

transactionCompletes = do
  setup
  transaction $ do
    insert_ comments [(def, Just "Kobayashi", c1)]
    insert_ comments
      [ (def, Nothing, "more anonymous spam")
      , (def, Just "Kobayashi", c2)
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
    insert_ comments [(def, Just "Kobayashi", c1)]
    insert_ comments
      [ (def, Nothing, "more anonymous spam")
      , (def, Just "Kobayashi", c2)
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
  insert_ people (fromRels peopleItems)
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

overrideAutoIncrement = do
  setup
  insert_ comments [(toRowId 123, Nothing, "hello")]
  num <- query $ aggregate $ do
    id :*: _ <- select comments
    restrict (id .== literal (toRowId 123))
    return (count id)
  assEq "failed to override auto-incrementing column" [1] num

insertAllDefaults = do
  setup
  pk <- insertWithPK comments [(def, def, def)]
  res <- query $ do
    comment@(id :*: _) <- select comments
    restrict (id .== literal pk)
    return comment
  assEq "wrong default values inserted" [pk :*: Nothing :*: ""] res

insertSomeDefaults = do
  setup
  insert_ people [Person "Celes" def (Just "chocobo") def]
  res <- query $ do
    person@(id :*: n :*: pet :*: c) <- select people
    restrict (pet .== just "chocobo")
    return person
  assEq "wrong values inserted" ["Celes" :*: 0 :*: Just "chocobo" :*: 0] res

weirdNames = do
  tryDropTable tableWithWeirdNames
  createTable tableWithWeirdNames
  i1 <- insert tableWithWeirdNames [(42, Nothing)]
  assEq "first insert failed" 1 i1
  i2 <- insert tableWithWeirdNames [(123, Just 321)]
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
    tableWithWeirdNames :: Table (Int, Maybe Int)
    tableWithWeirdNames =
      tableFieldMod "DROP TABLE comments" []
                    (<> "one \" quote \1\2\3\DEL\n two \"quotes\"")
    weird1 :*: weird2 = selectors tableWithWeirdNames

dupeInsertThrowsSeldaError = do
  tryDropTable comments'
  createTable comments'
  assertFail $ do
    insert_ comments'
      [ (0, Just "Kobayashi", "チョロゴン")
      , (0, Nothing, "some spam")
      ]
  dropTable comments'
  where
    comments' :: Table (Int, Maybe Text, Text)
    comments' = table "comments" [(\(x,_,_) -> x) :- primary]
    cId :*: cName :*: cComment = selectors comments

dupeInsert2ThrowsSeldaError = do
  setup
  insert_ comments [(def, Just "Kobayashi", "チョロゴン")]
  [ident :*: _] <- query $ limit 0 1 $ select comments
  e <- try $ insert_ comments [(ident, Nothing, "Spam, spam, spaaaaaam!")]
  case e :: Either SeldaError () of
    Left _ -> return ()
    _      -> liftIO $ assertFailure "SeldaError not thrown"

dupeUpdateThrowsSeldaError = do
  setup
  insert_ comments
    [ (def, Just "Kobayashi", "チョロゴン")
    , (def, Just "spammer"  , "some spam")
    ]
  [ident :*: _] <- query $ limit 0 1 $ select comments
  e <- try $ do
    update_ comments
      (\c -> c ! cName .== just "spammer")
      (\c -> c `with` [cId := literal ident])
  case e :: Either SeldaError () of
    Left _ -> return ()
    _      -> liftIO $ assertFailure "SeldaError not thrown"

nulQueries = do
  setup
  insert_ comments
    [ (def, Just "Kobayashi", "チョロゴン")
    , (def, Nothing         , "more \0 spam")
    , (def, Nothing         , "even more spam")
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

  insert_ comments [(def, Just "Link", "spam")]
  transaction $ do
    update_ comments
      (\c -> c ! cName .== just "Link")
      (\c -> c `with` [cComment := "insightful comment"])
    liftIO $ putMVar lock ()
    liftIO $ takeMVar lock
    insert_ addresses [(def, def)]

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
    assertFail $ insert_ addressesWithFK [("Nobody", "Nowhere")]
    dropTable addressesWithFK
  where
    addressesWithFK :: Table (Text, Text)
    addressesWithFK = table "addressesWithFK" [fst :- foreignKey people pName]

data FKAddrs = FKAddrs
  { fkaName :: Text
  , fkaCity :: Text
  } deriving Generic

genModFkViolationFails = do
    setup
    createTable addressesWithFK
    assertFail $ insert_ addressesWithFK [FKAddrs "Nobody" "Nowhere"]
    dropTable addressesWithFK
  where
    addressesWithFK :: Table FKAddrs
    addressesWithFK = tableFieldMod "addressesWithFK"
                                    [fkaName :- foreignKey people pName]
                                    ("test_" <>)

genModFkInsertSucceeds = do
    setup
    createTable addressesWithFK
    insert_ addressesWithFK [FKAddrs "Link" "Nowhere"]
    res <- query $ do
      (aName :*: aCity) <- select addressesWithFK
      person <- select people
      restrict (aName .== "Link" .&& aName .== person ! pName)
      return (person ! pName :*: aCity)
    assEq "wrong state after insert" ["Link" :*: "Nowhere"] res
    dropTable addressesWithFK
  where
    addressesWithFK :: Table FKAddrs
    addressesWithFK = tableFieldMod "addressesWithFK"
                                    [fkaName :- foreignKey people pName]
                                    ("test_" <>)

multipleFKs = do
    setup
    createTable addressesWithFK
    assertFail $ insert_ addressesWithFK [("Nobody", "Nowhere")]
    dropTable addressesWithFK
  where
    addressesWithFK :: Table (Text, Text)
    addressesWithFK = table "addressesWithFK"
      [ fst :- foreignKey people pName
      , fst :- foreignKey people pName
      ]

uniqueViolation = do
    createTable uniquePeople
    assertFail $ insert_ uniquePeople
      [ ("Link", Nothing)
      , ("Link", Nothing)
      ]
    r1 <- query $ select uniquePeople
    assertFail $ do
      insert_ uniquePeople [("Link", Nothing)]
      insert_ uniquePeople [("Link", Nothing)]
    r2 <- query $ select uniquePeople
    assEq "inserted rows despite constraint violation" [] r1
    assEq "row disappeared after violation" ["Link" :*: Nothing] r2
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text, Maybe Text)
    (uniquePeople, upName :*: upPet) =
          tableWithSelectors "uniquePeople" [fst :- unique]

insertOrUpdate = do
    tryDropTable counters
    createTable counters
    r1 <- upsert counters
           (\(c :*: v) -> c .== 0)
           (\(c :*: v) -> c :*: v+1)
           [(0, 1)]
    assEq "wrong return value from inserting upsert" (Just invalidRowId) r1

    r2 <- upsert counters
           (\(c :*: v) -> c .== 0)
           (\(c :*: v) -> c :*: v+1)
           [(0, 1)]
    assEq "wrong return value from updating upsert" Nothing r2

    res <- query $ select counters
    assEq "wrong value for counter" [0 :*: 2] res

    r3 <- upsert counters
           (\(c :*: v) -> c .== 15)
           (\(c :*: v) -> c :*: v+1)
           [(15, 1)]
    assEq "wrong return value from second inserting upsert" (Just invalidRowId) r3
    dropTable counters
  where
    counters :: Table (Int, Int)
    counters = table "counters" [fst :- primary]

tryInsertDoesntFail = do
    createTable uniquePeople
    res1 <- tryInsert uniquePeople [("Link", Nothing)]
    r1 <- query $ select uniquePeople
    res2 <- tryInsert uniquePeople [("Link", Nothing)]
    r2 <- query $ select uniquePeople
    assEq "wrong return value from successful tryInsert" True res1
    assEq "row not inserted" ["Link" :*: Nothing] r1
    assEq "wrong return value from failed tryInsert" False res2
    assEq "row inserted despite violation" ["Link" :*: Nothing] r2
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text, Maybe Text)
    (uniquePeople, upName :*: upPet) =
      tableWithSelectors "uniquePeople" [fst :- unique]

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

isInQuery = do
  setup
  res <- query $ do
    return (   "Link" `isIn` pName `from` select people
           :*: "Zelda" `isIn` pName `from` select people
           )
  assEq "wrong result from isIn" [True :*: False] res

blobColumn = do
    tryDropTable blobs
    createTable blobs
    n <- insert blobs [("b1", someBlob), ("b2", otherBlob)]
    assEq "wrong number of rows inserted" 2 n
    [k :*: v] <- query $ do
      (k :*: v) <- select blobs
      restrict (k .== "b1")
      return (k :*: v)
    assEq "wrong key for blob" "b1" k
    assEq "got wrong blob back" someBlob v
    dropTable blobs
  where
    blobs :: Table (Text, ByteString)
    blobs = table "blobs" []
    someBlob = "\0\1\2\3hello!漢字"
    otherBlob = "blah"

lazyBlobColumn = do
    tryDropTable blobs
    createTable blobs
    n <- insert blobs [("b1", someBlob), ("b2", otherBlob)]
    assEq "wrong number of rows inserted" 2 n
    [k :*: v] <- query $ do
      (k :*: v) <- select blobs
      restrict (k .== "b1")
      return (k :*: v)
    assEq "wrong key for blob" "b1" k
    assEq "got wrong blob back" someBlob v
    dropTable blobs
  where
    blobs :: Table (Text, Lazy.ByteString)
    blobs = table "blobs" []
    someBlob = "\0\1\2\3hello!漢字"
    otherBlob = "blah"

whenUnless = do
    setup

    insertUnless people (\t -> t ! pName .== "Lord Buckethead") theBucket
    oneBucket <- query $ select people `suchThat` ((.== "Lord Buckethead") . (! pName))
    assEq "Lord Buckethead wasn't inserted" theBucket (fromRels oneBucket)

    insertWhen people (\t -> t ! pName .== "Lord Buckethead") theSara
    oneSara <- query $ select people `suchThat` ((.== "Sara") . (! pName))
    assEq "Sara wasn't inserted" theSara (fromRels oneSara)

    insertUnless people (\t -> t ! pName .== "Lord Buckethead")
      [Person "Jessie" 16 Nothing (10^6)]
    noJessie <- query $ select people `suchThat` ((.== "Jessie") . (! pName))
    assEq "Jessie was wrongly inserted" [] (fromRels noJessie :: [Person])

    insertWhen people (\t -> t ! pName .== "Jessie")
      [Person "Lavinia" 16 Nothing (10^8)]
    noLavinia <- query $ select people `suchThat` ((.== "Lavinia") . (! pName))
    assEq "Lavinia was wrongly inserted" [] (fromRels noLavinia :: [Person])
    teardown
  where
    theBucket = [Person "Lord Buckethead" 30 Nothing 0]
    theSara = [Person "Sara" 14 Nothing 0]

manyParameters = do
    tryDropTable things
    createTable things
    inserted <- insert things [0..1000]
    actuallyInserted <- query $ aggregate $ count <$> select things
    dropTable things
    assEq "insert returned wrong insertion count" 1001 inserted
    assEq "wrong number of items inserted" [1001] actuallyInserted
  where
    things :: Table (Only Int)
    things = table "things" []

emptyInsert = do
  setup
  inserted <- insert people []
  assEq "wrong insertion count reported" 0 inserted
  teardown

boolTable = do
    tryDropTable tbl
    createTable tbl
    insert tbl [(def, True), (def, False), (def, def)]
    bs <- query $ second <$> select tbl
    assEq "wrong values inserted into table" [True, False, False] bs
    dropTable tbl
  where
    tbl :: Table (RowID, Bool)
    tbl = table "booltable" [fst :- untypedAutoPrimary]

optionalFK = do
    tryDropTable tbl
    createTable tbl
    pk <- insertWithPK tbl [(def, Nothing)]
    insert tbl [(def, Just pk)]
    vs <- query $ second <$> select tbl
    assEq "wrong value for nullable FK" [Nothing, Just pk] vs
    dropTable tbl
  where
    tbl :: Table (RowID, Maybe RowID)
    tbl = table "booltable" [fst :- untypedAutoPrimary, snd :- foreignKey tbl rid]
    (rid :*: _) = selectors tbl

-- | For genericAutoPrimary.
data AutoPrimaryUser = AutoPrimaryUser
  { uid :: ID AutoPrimaryUser
  , admin :: Bool

  , username :: Text
  , password :: Text

  , dateCreated :: UTCTime
  , dateModified :: UTCTime
  } deriving ( Eq, Show, Generic )

-- | For customEnum
data Foo = A | B | C | D
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

customEnum = do
    tryDropTable tbl
    createTable tbl

    inserted <- insert tbl [(def, A), (def, C), (def, C), (def, B)]
    assEq "wrong # of rows inserted" 4 inserted

    res <- query $ do
      _ :*: foo <- select tbl
      order foo descending
      return foo
    assEq "wrong pre-delete result list" [C, C, B, A] res

    deleted <- deleteFrom tbl ((.== literal C) . second)
    assEq "wrong # of rows deleted" 2 deleted

    res2 <- query $ do
      _ :*: foo <- select tbl
      order foo ascending
      return foo
    assEq "wrong post-delete result list" [A, B] res2

    dropTable tbl
  where
    tbl :: Table (RowID, Foo)
    tbl = table "enums" [fst :- untypedAutoPrimary]

genericTupleTable = do
    tryDropTable tbl
    tryCreateTable tbl
    insert tbl [ (0, Nested $ Person "A" 1 Nothing 2)
               , (1, Nested $ Person "B" 3 (Just "C") 3)]
    res <- query $ do
      a <- select tbl
      order (a ! s_age) descending
      return a
    let res' = fromRels res :: [(Int, Nested Person)]
    assEq "Wrong result query against tuple table." desc_persons (map snd res')
    dropTable tbl
  where
    desc_persons =
      [ Nested $ Person "B" 3 (Just "C") 3
      , Nested $ Person "A" 1 Nothing 2
      ]
    tbl :: Table (Int, Nested Person)
    tbl = table "someRandomTuple" [fst :- primary]
    s_id :*: s_name :*: s_age :*: s_pet :*: s_cash = selectors tbl

disableForeignKeys = do
    -- Run the test twice, to check that FK checking gets turned back on again
    -- properly.
    go ; go
  where
    go = do
      tryDropTable tbl2
      tryDropTable tbl1
      createTable tbl1
      createTable tbl2
      pk <- insertWithPK tbl1 [Only def]
      insert tbl2 [(def, pk)]
      assertFail $ dropTable tbl1
      withoutForeignKeyEnforcement $ dropTable tbl1 >> dropTable tbl2
      tryDropTable tbl2
      tryDropTable tbl1

    tbl1 :: Table (Only RowID)
    tbl1 = table "table1" [the :- untypedAutoPrimary]
    id1 = selectors tbl1

    tbl2 :: Table (RowID, RowID)
    tbl2 = table "table2" [fst :- untypedAutoPrimary, snd :- foreignKey tbl1 id1]

migrationTest test = do
    tryDropTable migrationTable1
    createTable migrationTable1
    insert_ migrationTable1 [1,2,3]
    test
    tryDropTable migrationTable1
    tryDropTable migrationTable2
    tryDropTable migrationTable3

migrationTable1 :: Table (Only Int)
migrationTable1 = table "table1" [the :- primary]

migrationTable2 :: Table (Text, Int)
migrationTable2 = table "table1" [fst :- primary]

migrationTable3 :: Table (Only Int)
migrationTable3 = table "table3" [the :- primary]

steps =
  [ [Migration migrationTable1 migrationTable1 pure]
  , [Migration migrationTable1 migrationTable2 (\foo -> pure (toString foo :*: foo))]
  , [Migration migrationTable2 migrationTable3 (\(txt :*: i) -> pure i)]
  , [Migration migrationTable3 migrationTable1 pure]
  ]

migrateIntoSelf = do
  migrate migrationTable1 migrationTable1 id
  res <- query $ do
    x <- select migrationTable1
    order x ascending
    return x
  assEq "migrating into self went wrong" [1,2,3] res

addColumn = do
  migrate migrationTable1 migrationTable2 $ \foo -> toString foo :*: foo
  res <- query $ do
    x :*: y <- select migrationTable2
    order x ascending
    return (x :*: y)
  assEq "adding column went wrong" ["1":*:1,"2":*:2,"3":*:3] res

dropColumn = do
  migrate migrationTable1 migrationTable2 $ \foo -> toString foo :*: foo
  migrate migrationTable2 migrationTable3 $ \(txt :*: i) -> i
  assertFail $ query $ select migrationTable2
  res <- query $ do
    x <- select migrationTable3
    order x ascending
    return x
  assEq "migrating back went wrong" [1,2,3] res

autoMigrateOneStep = do
  migrate migrationTable1 migrationTable3 id
  autoMigrate False steps
  res <- query $ do
    x <- select migrationTable1
    order x ascending
    return x
  assEq "automigration failed" [1,2,3] res

autoMigrateNoOp = do
  autoMigrate True steps
  res <- query $ do
    x <- select migrationTable1
    order x ascending
    return x
  assEq "no-op automigration failed" [1,2,3] res

migrateAggregate = do
  setup
  migrateM migrationTable1 migrationTable2 $ \foo -> do
    age <- aggregate $ do
      person <- select people
      return $ min_ (person ! pAge)
    return $ toString foo :*: age
  res <- query $ do
    x :*: y <- select migrationTable2
    order y ascending
    return (x :*: y)
  assEq "query migration failed" ["1":*:10,"2":*:10,"3":*:10] res

autoMigrateMultiStep = do
  autoMigrate True steps
  res <- query $ do
    x <- select migrationTable1
    order x ascending
    return x
  assEq "multi-step automigration failed" [1,2,3] res
