{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE TypeOperators, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, CPP #-}
-- | Tests that modify the database.
module Tests.Mutable (mutableTests) where
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
import Database.Selda.MakeSelectors
import Database.Selda.Validation (validateTable)
import Database.Selda.Unsafe (unsafeSelector, rawStm)
import Test.HUnit
import Utils
import Tables
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup
#endif

mutableTests :: (SeldaM b () -> IO ()) -> Test
mutableTests freshEnv = test
  [ "tryDrop never fails"            ~: freshEnv tryDropNeverFails
  , "tryCreate never fails"          ~: freshEnv tryCreateNeverFails
  , "drop fails on missing"          ~: freshEnv dropFailsOnMissing
  , "create fails on duplicate"      ~: freshEnv createFailsOnDuplicate
  , "auto primary increments"        ~: freshEnv (autoPrimaryIncrements comments)
  , "insert returns number of rows"  ~: freshEnv insertReturnsNumRows
  , "update updates table"           ~: freshEnv updateUpdates
  , "update nothing"                 ~: freshEnv updateNothing
  , "insert time values"             ~: freshEnv insertTime
  , "transaction completes"          ~: freshEnv transactionCompletes
  , "transaction rolls back"         ~: freshEnv transactionRollsBack
  , "queries are consistent"         ~: freshEnv consistentQueries
  , "delete deletes"                 ~: freshEnv deleteDeletes
  , "delete everything"              ~: freshEnv deleteEverything
  , "override auto-increment"        ~: freshEnv (overrideAutoIncrement comments)
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
  , "disable foreign key checks"     ~: freshEnv disableForeignKeys
  , "mod fk violation fails"         ~: freshEnv genModFkViolationFails
  , "mod fk insertion ok"            ~: freshEnv genModFkInsertSucceeds
  , "migrate into self"              ~: freshEnv (migrationTest migrateIntoSelf)
  , "drop column migration"          ~: freshEnv (migrationTest dropColumn)
  , "auto-migrate one step"          ~: freshEnv (migrationTest autoMigrateOneStep)
  , "auto-migrate no-op"             ~: freshEnv (migrationTest autoMigrateNoOp)
  , "migrate aggregate"              ~: freshEnv (migrationTest migrateAggregate)
  , "auto-migrate multi-step"        ~: freshEnv (migrationTest autoMigrateOneStep)
  , "multi-unique insert"            ~: freshEnv multiUnique
  , "uuid inserts"                   ~: freshEnv uuidInserts
  , "uuid queries"                   ~: freshEnv uuidQueries
  , "migrate table with index"       ~: freshEnv migrateIndex
  , "weak auto primary increments"   ~: freshEnv (autoPrimaryIncrements weakComments)
  , "override weak auto-increment"   ~: freshEnv (overrideAutoIncrement weakComments)
  , "disable FKs with rawStm"        ~: freshEnv disableFKsWithRawStm
  , "overwrite row on update"        ~: freshEnv overwriteRow
  ]

tryDropNeverFails :: SeldaM b ()
tryDropNeverFails = teardown

tryCreateNeverFails :: SeldaM b ()
tryCreateNeverFails = tryCreateTable comments >> tryCreateTable comments

dropFailsOnMissing = assertFail $ dropTable comments
createFailsOnDuplicate = createTable people >> assertFail (createTable people)

autoPrimaryIncrements c = do
  setup
  k <- untyped <$> insertWithPK c [(def, Just "Kobayashi", "チョロゴン")]
  k' <- untyped <$> insertWithPK c [(def, Nothing, "more anonymous spam")]
  [name] <- query $ do
    t <- select c
    restrict (t!cId .== literal k)
    return (t!cName)
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
  rows <- update comments (isNull . (!cName))
                          (`with` [cName := just "anon"])
  [upd] <- query $ aggregate $ do
    t <- select comments
    restrict (not_ $ isNull (t!cName))
    restrict (t!cName .== just "anon")
    return (count (t!cName))
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
  let Just t = parseTimeM True defaultTimeLocale "%F %H:%M:%S%Q" "2011-11-11 11:11:11.11111"
      Just d = parseTimeM True defaultTimeLocale "%F" "2011-11-11"
      Just lt = parseTimeM True defaultTimeLocale "%H:%M:%S%Q" "11:11:11.11111"
  insert_ times [("now", t, d, lt)]
  [("now", t', d', lt')] <- query $ select times
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
    t <- select comments
    restrict (t!cName .== just "Kobayashi")
    return (t!cComment)
  ass "some inserts were not performed"
      (c1 `elem` cs && c2 `elem` cs && length cs == 2)
  where
    c1 = "チョロゴン"
    c2 = "メイド最高！"

transactionRollsBack :: SeldaM b ()
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
        t <- select comments
        restrict (t!cName .== just "Kobayashi")
        return (t!cComment)
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
      t <- select people
      restrict (round_ (t!pCash) .> (t!pAge))
      return (t!pName)

deleteDeletes = do
  setup
  a <- query q
  deleteFrom_ people (\t -> t!pName .== "Velvet")
  b <- query q
  ass "rows not deleted" (a /= b && length b < length a)
  where
    q = do
      t <- select people
      restrict (round_ (t!pCash) .< (t!pAge))
      return (t!pName)

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
      t <- select people
      restrict (round_ (t!pCash) .> (t!pAge))
      return (t!pName)

overrideAutoIncrement c = do
  setup
  insert_ c [(toRowId 123, Nothing, "hello")]
  num <- query $ aggregate $ do
    t <- select c
    restrict (t!cId .== literal (toRowId 123))
    return (count (t!cId))
  assEq "failed to override auto-incrementing column" [1] num

insertAllDefaults = do
  setup
  pk <- untyped <$> insertWithPK comments [(def, def, def)]
  res <- query $ do
    comment <- select comments
    restrict (comment!cId .== literal pk)
    return comment
  assEq "wrong default values inserted" [(pk, Nothing, "")] res

insertSomeDefaults = do
  setup
  insert_ people [Person "Celes" def (Just "chocobo") def]
  res <- query $ do
    person <- select people
    restrict (person!pPet .== just "chocobo")
    return person
  assEq "wrong values inserted" [Person "Celes" 0 (Just "chocobo") 0] res

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
    comments' = table "comments" [Single cId :- primary]
    cId :*: cName :*: cComment = selectors comments'

dupeInsert2ThrowsSeldaError :: SeldaM b ()
dupeInsert2ThrowsSeldaError = do
  setup
  insert_ comments [(def, Just "Kobayashi", "チョロゴン")]
  [(ident, _, _)] <- query $ limit 0 1 $ select comments
  e <- try $ insert_ comments [(ident, Nothing, "Spam, spam, spaaaaaam!")]
  case e :: Either SeldaError () of
    Left _ -> return ()
    _      -> liftIO $ assertFailure "SeldaError not thrown"

dupeUpdateThrowsSeldaError :: SeldaM b ()
dupeUpdateThrowsSeldaError = do
  setup
  insert_ comments
    [ (def, Just "Kobayashi", "チョロゴン")
    , (def, Just "spammer"  , "some spam")
    ]
  [(ident, _, _)] <- query $ limit 0 1 $ select comments
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
  rows <- update comments (isNull . (!cName))
                          (`with` [cName := just "\0"])
  [upd] <- query $ aggregate $ do
    t <- select comments
    restrict (not_ $ isNull (t!cName))
    restrict (t!cName .== just "\0")
    return (count (t!cName))
  assEq "update returns wrong number of updated rows" 3 rows
  assEq "rows were not updated" 3 upd

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
    addressesWithFK = table "addressesWithFK" [one :- foreignKey people pName]
    one :*: two = selectors addressesWithFK

data FKAddrs = FKAddrs
  { fkaName :: Text
  , fkaCity :: Text
  } deriving Generic
instance SqlRow FKAddrs

genModFkViolationFails = do
    setup
    createTable addressesWithFK
    assertFail $ insert_ addressesWithFK [FKAddrs "Nobody" "Nowhere"]
    dropTable addressesWithFK
  where
    addressesWithFK :: Table FKAddrs
    addressesWithFK = tableFieldMod "addressesWithFK"
                                    [aName :- foreignKey people pName]
                                    ("test_" <>)
    aName :*: aCity = selectors addressesWithFK

genModFkInsertSucceeds = do
    setup
    createTable addressesWithFK
    insert_ addressesWithFK [FKAddrs "Link" "Nowhere"]
    res <- query $ do
      t <- select addressesWithFK
      person <- select people
      restrict (t!aName .== "Link" .&& t!aName .== person ! pName)
      return (person!pName :*: t!aCity)
    assEq "wrong state after insert" ["Link" :*: "Nowhere"] res
    dropTable addressesWithFK
  where
    addressesWithFK :: Table FKAddrs
    addressesWithFK = tableFieldMod "addressesWithFK"
                                    [aName :- foreignKey people pName]
                                    ("test_" <>)
    aName :*: aCity = selectors addressesWithFK

multipleFKs = do
    setup
    createTable addressesWithFK
    assertFail $ insert_ addressesWithFK [("Nobody", "Nowhere")]
    dropTable addressesWithFK
  where
    addressesWithFK :: Table (Text, Text)
    addressesWithFK = table "addressesWithFK"
      [ one :- foreignKey people pName
      , two :- foreignKey people pName
      ]
    one :*: two = selectors addressesWithFK

uniqueViolation = do
    tryDropTable uniquePeople
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
    assEq "row disappeared after violation" [("Link", Nothing)] r2
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text, Maybe Text)
    (uniquePeople, upName :*: upPet) =
          tableWithSelectors "uniquePeople" [Single upName :- unique]

insertOrUpdate = do
    tryDropTable counters
    createTable counters
    r1 <- fmap untyped <$> upsert counters
           (\t -> t!c .== 0)
           (\t -> t `with` [v += 1])
           [(0, 1)]
    assEq "wrong return value from inserting upsert" (Just invalidRowId) r1

    r2 <- fmap untyped <$> upsert counters
           (\t -> t!c .== 0)
           (\t -> t `with` [v $= (+1)])
           [(0, 1)]
    assEq "wrong return value from updating upsert" Nothing r2

    res <- query $ select counters
    assEq "wrong value for counter" [(0, 2)] res

    r3 <- fmap untyped <$> upsert counters
           (\t -> t ! c .== 15)
           (\t -> t `with` [v := t!v + 1])
           [(15, 1)]
    assEq "wrong return value from second inserting upsert" (Just invalidRowId) r3
    dropTable counters
  where
    counters :: Table (Int, Int)
    counters = table "counters" [Single c :- primary]
    c :*: v = selectors counters

tryInsertDoesntFail = do
    createTable uniquePeople
    res1 <- tryInsert uniquePeople [("Link", Nothing)]
    r1 <- query $ select uniquePeople
    res2 <- tryInsert uniquePeople [("Link", Nothing)]
    r2 <- query $ select uniquePeople
    assEq "wrong return value from successful tryInsert" True res1
    assEq "row not inserted" [("Link", Nothing)] r1
    assEq "wrong return value from failed tryInsert" False res2
    assEq "row inserted despite violation" [("Link", Nothing)] r2
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text, Maybe Text)
    (uniquePeople, upName :*: upPet) =
      tableWithSelectors "uniquePeople" [Single upName :- unique]

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
    [(k, v)] <- query $ do
      t <- select blobs
      restrict (t ! ks .== "b1")
      return t
    assEq "wrong key for blob" "b1" k
    assEq "got wrong blob back" someBlob v
    dropTable blobs
  where
    blobs :: Table (Text, ByteString)
    blobs = table "blobs" []
    ks :*: vs = selectors blobs
    someBlob = "\0\1\2\3hello!漢字"
    otherBlob = "blah"

lazyBlobColumn = do
    tryDropTable blobs
    createTable blobs
    n <- insert blobs [("b1", someBlob), ("b2", otherBlob)]
    assEq "wrong number of rows inserted" 2 n
    [(k, v)] <- query $ do
      t <- select blobs
      restrict (t ! ks .== "b1")
      return t
    assEq "wrong key for blob" "b1" k
    assEq "got wrong blob back" someBlob v
    dropTable blobs
  where
    blobs :: Table (Text, Lazy.ByteString)
    blobs = table "blobs" []
    ks :*: vs = selectors blobs
    someBlob = "\0\1\2\3hello!漢字"
    otherBlob = "blah"

whenUnless = do
    setup

    insertUnless people (\t -> t ! pName .== "Lord Buckethead") theBucket
    oneBucket <- query $ select people `suchThat` ((.== "Lord Buckethead") . (! pName))
    assEq "Lord Buckethead wasn't inserted" theBucket (oneBucket)

    insertWhen people (\t -> t ! pName .== "Lord Buckethead") theSara
    oneSara <- query $ select people `suchThat` ((.== "Sara") . (! pName))
    assEq "Sara wasn't inserted" theSara (oneSara)

    insertUnless people (\t -> t ! pName .== "Lord Buckethead")
      [Person "Jessie" 16 Nothing (10^6)]
    noJessie <- query $ select people `suchThat` ((.== "Jessie") . (! pName))
    assEq "Jessie was wrongly inserted" [] (noJessie :: [Person])

    insertWhen people (\t -> t ! pName .== "Jessie")
      [Person "Lavinia" 16 Nothing (10^8)]
    noLavinia <- query $ select people `suchThat` ((.== "Lavinia") . (! pName))
    assEq "Lavinia was wrongly inserted" [] (noLavinia :: [Person])
    teardown
  where
    theBucket = [Person "Lord Buckethead" 30 Nothing 0]
    theSara = [Person "Sara" 14 Nothing 0]

manyParameters = do
    tryDropTable things
    createTable things
    inserted <- insert things [0..1000]
    actuallyInserted <- query $ aggregate $ count . the <$> select things
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
    bs <- query $ (! two) <$> select tbl
    assEq "wrong values inserted into table" [True, False, False] bs
    dropTable tbl
  where
    tbl :: Table (RowID, Bool)
    tbl = table "booltable" [one :- untypedAutoPrimary]
    one :*: two = selectors tbl

optionalFK = do
    tryDropTable tbl
    createTable tbl
    pk <- untyped <$> insertWithPK tbl [(def, Nothing)]
    insert tbl [(def, Just pk)]
    vs <- query $ (! mrid) <$> select tbl
    assEq "wrong value for nullable FK" [Nothing, Just pk] vs
    dropTable tbl
  where
    tbl :: Table (RowID, Maybe RowID)
    tbl = table "booltable" [rid :- untypedAutoPrimary, mrid :- foreignKey tbl rid]
    (rid :*: mrid) = selectors tbl

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
instance SqlType Foo

customEnum = do
    tryDropTable tbl
    createTable tbl

    inserted <- insert tbl [(def, A), (def, C), (def, C), (def, B)]
    assEq "wrong # of rows inserted" 4 inserted

    res <- query $ do
      t <- select tbl
      order (t ! two) descending
      return (t ! two)
    assEq "wrong pre-delete result list" [C, C, B, A] res

    deleted <- deleteFrom tbl ((.== literal C) . (! two))
    assEq "wrong # of rows deleted" 2 deleted

    res2 <- query $ do
      t <- select tbl
      order (t ! two) ascending
      return (t ! two)
    assEq "wrong post-delete result list" [A, B] res2

    dropTable tbl
  where
    tbl :: Table (RowID, Foo)
    tbl = table "enums" [one :- untypedAutoPrimary]
    one :*: two = selectors tbl

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
      pk <- untyped <$> insertWithPK tbl1 [Only def]
      insert tbl2 [(def, pk)]
      assertFail $ dropTable tbl1
      withoutForeignKeyEnforcement $ dropTable tbl1 >> dropTable tbl2
      tryDropTable tbl2
      tryDropTable tbl1

    tbl1 :: Table (Only RowID)
    tbl1 = table "table1" [id1 :- untypedAutoPrimary]
    id1 = selectors tbl1

    tbl2 :: Table (RowID, RowID)
    tbl2 = table "table2" [s_fst :- untypedAutoPrimary, s_snd :- foreignKey tbl1 id1]
    s_fst :*: s_snd = selectors tbl2

migrationTest test = do
    tryDropTable migrationTable1
    createTable migrationTable1
    insert_ migrationTable1 [1,2,3]
    test
    tryDropTable migrationTable1
    tryDropTable migrationTable2
    tryDropTable migrationTable3

migrationTable1 :: Table (Only Int)
migrationTable1 = table "table1" [Single mt1_1 :- primary]
mt1_1 = selectors migrationTable1

migrationTable2 :: Table (Text, Int)
migrationTable2 = table "table1" [Single mt2_1 :- primary]
mt2_1 :*: mt2_2 = selectors migrationTable2

migrationTable3 :: Table (Only Int)
migrationTable3 = table "table3" [Single mt3_1 :- primary]
mt3_1 = selectors migrationTable3

steps =
  [ [Migration migrationTable1 migrationTable1 pure]
  , [Migration migrationTable1 migrationTable2 $ \foo -> pure $ new
      [ mt2_1 := toString (the foo)
      , mt2_2 := the foo
      ]
    ]
  , [Migration migrationTable2 migrationTable3 $ \t -> pure (only (t ! mt2_2))]
  , [Migration migrationTable3 migrationTable1 pure]
  ]

migrateIntoSelf = do
  migrate migrationTable1 migrationTable1 id
  res <- query $ do
    x <- select migrationTable1
    order (the x) ascending
    return x
  assEq "migrating into self went wrong" [1,2,3] res

addColumn = do
  migrate migrationTable1 migrationTable2 $ \foo -> new
    [ mt2_1 := toString (the foo)
    , mt2_2 := the foo
    ]
  res <- query $ do
    t <- select migrationTable2
    order (t ! mt2_1) ascending
    return t
  assEq "adding column went wrong" [("1",1),("2",2),("3",3)] res

dropColumn = do
  migrate migrationTable1 migrationTable2 $ \foo -> new
    [ mt2_1 := toString (the foo)
    , mt2_2 := the foo
    ]
  migrate migrationTable2 migrationTable3 $ \tbl -> only (tbl ! mt2_2)
  assertFail $ query $ select migrationTable2
  res <- query $ do
    x <- select migrationTable3
    order (the x) ascending
    return x
  assEq "migrating back went wrong" [1,2,3] res

autoMigrateOneStep = do
  migrate migrationTable1 migrationTable3 id
  autoMigrate False steps
  res <- query $ do
    x <- select migrationTable1
    order (the x) ascending
    return x
  assEq "automigration failed" [1,2,3] res

autoMigrateNoOp = do
  autoMigrate True steps
  res <- query $ do
    x <- select migrationTable1
    order (the x) ascending
    return x
  assEq "no-op automigration failed" [1,2,3] res

migrateAggregate = do
  setup
  migrateM migrationTable1 migrationTable2 $ \foo -> do
    age <- aggregate $ do
      person <- select people
      return $ ifNull 0 .<$> min_ (person ! pAge)
    return $ new [mt2_1 := toString (the foo), mt2_2 := age]
  res <- query $ do
    t <- select migrationTable2
    order (t ! mt2_2) ascending
    return t
  assEq "query migration failed" [("1",10),("2",10),("3",10)] res

autoMigrateMultiStep = do
  autoMigrate True steps
  res <- query $ do
    x <- select migrationTable1
    order (the x) ascending
    return x
  assEq "multi-step automigration failed" [1,2,3] res

multiUnique = do
    tryDropTable uniques
    createTable uniques
    insert_ uniques [(1,1), (1,2), (2,1), (2,2)]
    expectFalse1 <- tryInsert uniques [(1,1)]
    expectFalse2 <- tryInsert uniques [(1,2)]
    expectTrue1 <- tryInsert uniques [(1,3)]
    expectTrue2 <- tryInsert uniques [(3,3)]
    assEq "uniqueness violation" False expectFalse1
    assEq "uniqueness violation" False expectFalse2
    assEq "overly strict uniqueness constraint" True expectTrue1
    assEq "overly strict uniqueness constraint" True expectTrue2
    dropTable uniques
  where
    uniques :: Table (Int, Int)
    (uniques, ua :*: ub) =
      tableWithSelectors "uniques" [(ua :+ Single ub) :- unique]

uuidTable :: Table (UUID, Int)
uuidTable = table "uuidTable"
  [ Single (unsafeSelector 0 :: Selector (UUID, Int) UUID) :- primary
  ]

uuidSetup = do
  tryDropTable uuidTable
  createTable uuidTable
  uuid <- newUuid
  assertFail $ insert_ uuidTable [(uuid, 1), (uuid, 2)]
  uuid2 <- newUuid
  insert_ uuidTable [(uuid, 1), (uuid2, 2)]
  return (uuid, uuid2)

uuidInserts = do
  _ <- uuidSetup
  dropTable uuidTable

uuidQueries = do
  (a, b) <- uuidSetup
  [(a', n)] <- query $ do
    x <- select uuidTable
    restrict (x ! unsafeSelector 0 .== literal a)
    return x
  dropTable uuidTable
  assEq "wrong uuid returned" a a'

migrateIndex :: SeldaM b ()
migrateIndex = do
    tryDropTable tbl1
    createTable tbl1
    migrate tbl1 tbl2 (\x -> new [a2 := x ! a1, b := 0])
    validateTable tbl2
    migrate tbl2 tbl1 (\x -> new [a1 := x ! a2])
    validateTable tbl1
    dropTable tbl1
  where
    tbl1 :: Table (Only Int)
    (tbl1, a1) = tableWithSelectors "foo" [Single a1 :- index]

    tbl2 :: Table (Int, Int)
    (tbl2, a2 :*: b) = tableWithSelectors "foo" [Single a2 :- index]

disableFKsWithRawStm :: SeldaM b ()
disableFKsWithRawStm = do
    createTable people
    createTable tbl
    assertFail $ insert_ tbl [("nonexistent person", "asdas")]
#ifdef SQLITE
    rawStm "PRAGMA foreign_keys = OFF"
#endif
#ifdef POSTGRES
    rawStm "ALTER TABLE fkaddrs DISABLE TRIGGER ALL"
#endif
    n <- insert tbl [("nonexistent person", "asdas")]
    assEq "failed to insert bad person" 1 n
    dropTable tbl
    dropTable people
#ifdef SQLITE
    rawStm "PRAGMA foreign_keys = OFF"
#endif
  where
    tbl = table "fkaddrs" [aName :- foreignKey people pName]

overwriteRow :: SeldaM b ()
overwriteRow = do
    createTable people
    insert people [p1]
    update people (\p -> p!pName .== "Testingway") (const (row p2))
    ps <- query $ select people
    assEq "row not overwritten" [p2] ps
  where
    p1 = Person "Testingway" 101 Nothing 0.2
    p2 = Person "Changingway" 99 (Just "Pet Rock") 100
