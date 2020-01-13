{-# LANGUAGE TypeOperators, OverloadedStrings #-}
-- | Schema validation tests.
module Tests.Validation (validationTests) where
import Control.Concurrent
import Control.Monad.Catch
import Data.List hiding (groupBy, insert)
import Data.Time
import Database.Selda
import Database.Selda.Unsafe
import Database.Selda.Validation
import Database.Selda.MakeSelectors
import Database.Selda.Backend
import Test.HUnit
import Utils
import Tables

validationTests :: (SeldaM b () -> IO ()) -> [Test]
validationTests freshEnv =
  [ "nul identifiers fail"                ~: freshEnv nulIdentifiersFail
  , "empty identifiers are caught"        ~: freshEnv emptyIdentifiersFail
  , "duplicate columns are caught"        ~: freshEnv duplicateColsFail
  , "duplicate PKs are caught"            ~: freshEnv duplicatePKsFail
  , "non-unique FK fails"                 ~: freshEnv nonUniqueFKFails
  , "non-primary unique FK passes"        ~: freshEnv nonPrimaryUniqueFK
  , "nullable unique field passes"        ~: freshEnv nullableUnique
  , "validating wrong table fails"        ~: freshEnv validateWrongTable
  , "validating nonexistent table fails"  ~: freshEnv validateNonexistentTable
  , "multi-column unique validation"      ~: freshEnv validateMultiUnique
  , "single-column unique validation"     ~: freshEnv validateSingleUnique
  , "multi-column primary key validation" ~: freshEnv validateMultiPk
  , "timestamp column validation"         ~: freshEnv validateTimestamp
  , "auto-incrementing PK validation"     ~: freshEnv validateAutoPrimary
  ]

nulIdentifiersFail = do
  e1 <- try (createTable nulTable) :: SeldaM b (Either ValidationError ())
  e2 <- try (createTable nulColTable) :: SeldaM b (Either ValidationError ())
  case (e1, e2) of
    (Left _, Left _) -> return ()
    _                -> liftIO $ assertFailure "ValidationError not thrown"
  where
    nulTable :: Table (Int, Int)
    nulTable = table "table_\0" []

    nulColTable :: Table (Int, Int)
    nulColTable = tableFieldMod "nul_col_table" [] (const "col_\0")

emptyIdentifiersFail = do
  e1 <- try (createTable noNameTable) :: SeldaM b (Either ValidationError ())
  e2 <- try (createTable noColNameTable) :: SeldaM b (Either ValidationError ())
  case (e1, e2) of
    (Left _, Left _)   -> return ()
    (Right _, Left _)  -> liftIO $ assertFailure "empty table name not caught"
    (Left _, Right _)  -> liftIO $ assertFailure "empty col name not caught"
    (Right _, Right _) -> liftIO $ assertFailure "no empty name caught"
  where
    noNameTable :: Table (Int, Int)
    noNameTable = table "" []

    noColNameTable :: Table (Int, Int)
    noColNameTable = tableFieldMod "table with empty col name" [] (const "")

duplicateColsFail = do
  e <- try (createTable dupes) :: SeldaM b (Either ValidationError ())
  case e of
    Left _ -> return ()
    _      -> liftIO $ assertFailure "ValidationError not thrown"
  where
    dupes :: Table (Int, Text)
    dupes = tableFieldMod "duplicate" [] (const "blah")

sel_fst :: (SqlType a, SqlType b) => Selector (a, b) a
sel_fst = unsafeSelector 0

sel_snd :: (SqlType a, SqlType b) => Selector (a, b) b
sel_snd = unsafeSelector 1

duplicatePKsFail = do
  e1 <- try (createTable dupes1) :: SeldaM b (Either ValidationError ())
  e2 <- try (createTable dupes2) :: SeldaM b (Either ValidationError ())
  case (e1, e2) of
    (Left _, Left _) -> return ()
    _                -> liftIO $ assertFailure "ValidationError not thrown"
  where
    dupes1 :: Table (Int, Text)
    dupes1 = table "duplicate"
      [ Single sel_fst :- primary
      , Single sel_snd :- primary
      ]
    dupes2 :: Table (RowID, Text)
    dupes2 = table "duplicate"
      [ sel_fst :- untypedAutoPrimary
      , Single sel_snd :- primary
      ]

nonUniqueFKFails = do
    res <- try (createTable addressesWithFK) :: SeldaM b (Either ValidationError ())
    case res of
      Left _  -> return ()
      Right _ -> liftIO $ assertFailure "ValidationError not thrown"
  where
    addressesWithFK :: Table (Text, Text)
    addressesWithFK = table "addressesWithFK"
      [ sel_fst :- foreignKey comments cComment
      ]

nonPrimaryUniqueFK :: SeldaM b ()
nonPrimaryUniqueFK = do
    tryDropTable addressesWithFK
    tryDropTable uniquePeople

    createTable uniquePeople
    createTable addressesWithFK

    validateTable uniquePeople
    validateTable addressesWithFK

    dropTable addressesWithFK
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text, Maybe Text)
    (uniquePeople, upName :*: upPet) =
      tableWithSelectors "uniquePeople" [Single upName :- unique]
    addressesWithFK :: Table (Text, Text)
    addressesWithFK = table "addressesWithFK"
      [ sel_fst :- foreignKey uniquePeople upName
      ]

nullableUnique :: SeldaM b ()
nullableUnique = do
    tryDropTable uniquePeople
    createTable uniquePeople
    validateTable uniquePeople
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text, Maybe Text)
    (uniquePeople, upName :*: upPet) =
      tableWithSelectors "uniquePeople"
        [ Single upName :- unique
        , Single upPet :- unique
        ]

validateWrongTable = do
    tryDropTable goodPeople
    createTable goodPeople
    assertFail $ validateTable badPeople1
    assertFail $ validateTable badPeople2
    assertFail $ validateTable badPeople3
    assertFail $ validateTable badPeople4
    assertFail $ validateTable badIxPeople1
    assertFail $ validateTable badIxPeople2
    assertFail $ validateTable badIxPeople3
    assertFail $ validateTable badIxPeople4
    dropTable goodPeople
  where
    goodPeople :: Table (Text, Int, Maybe Text, Double)
    goodPeople = table "people"
      [ (Single (unsafeSelector 0 :: Selector (Text, Int, Maybe Text, Double) Text))
          :- index
      , (Single (unsafeSelector 3 :: Selector (Text, Int, Maybe Text, Double) Double))
          :- indexUsing HashIndex
      ]

    badPeople1 :: Table (Text, Int, Text, Double)
    badPeople1 = table "people"
      [ (Single (unsafeSelector 0 :: Selector (Text, Int, Text, Double) Text))
          :- index
      , (Single (unsafeSelector 3 :: Selector (Text, Int, Text, Double) Double))
          :- indexUsing HashIndex
      ]

    badPeople2 :: Table (Text, Bool, Maybe Text, Double)
    badPeople2 = table "people"
      [ (Single (unsafeSelector 0 :: Selector (Text, Bool, Maybe Text, Double) Text)) :- index
      , (Single (unsafeSelector 3 :: Selector (Text, Bool, Maybe Text, Double) Double)) :- indexUsing HashIndex
      ]

    badPeople3 :: Table (Text, Int, Maybe Text)
    badPeople3 = table "people"
      [ (Single (unsafeSelector 0 :: Selector (Text, Int, Maybe Text) Text)) :- index
      ]

    badPeople4 :: Table (Text, Int, Maybe Text, Double, Int)
    badPeople4 = table "people"
      [ (Single (unsafeSelector 0 :: Selector (Text, Int, Maybe Text, Double, Int) Text)) :- index
      , (Single (unsafeSelector 3 :: Selector (Text, Int, Maybe Text, Double, Int) Double)) :- indexUsing HashIndex
      ]

    badIxPeople1 :: Table (Text, Int, Maybe Text, Double)
    badIxPeople1 = table "people"
      [ (Single (unsafeSelector 3 :: Selector (Text, Int, Maybe Text, Double) Double)) :- indexUsing HashIndex
      ]

    badIxPeople2 :: Table (Text, Int, Maybe Text, Double)
    badIxPeople2 = table "people"
      [ (Single (unsafeSelector 2 :: Selector (Text, Int, Maybe Text, Double) (Maybe Text))) :- index
      ]

    badIxPeople3 :: Table (Text, Int, Maybe Text, Double)
    badIxPeople3 = table "people"
      [ (Single (unsafeSelector 0 :: Selector (Text, Int, Maybe Text, Double) Text)) :- index
      , (Single (unsafeSelector 2 :: Selector (Text, Int, Maybe Text, Double) (Maybe Text))) :- index
      , (Single (unsafeSelector 3 :: Selector (Text, Int, Maybe Text, Double) Double)) :- indexUsing HashIndex
      ]

    badIxPeople4 :: Table (Text, Int, Maybe Text, Double)
    badIxPeople4 = table "people"
      [ (Single (unsafeSelector 1 :: Selector (Text, Int, Maybe Text, Double) Int)) :- index
      , (Single (unsafeSelector 2 :: Selector (Text, Int, Maybe Text, Double) (Maybe Text))) :- indexUsing HashIndex
      , (Single (unsafeSelector 3 :: Selector (Text, Int, Maybe Text, Double) Double)) :- indexUsing HashIndex
      ]

validateNonexistentTable = do
    assertFail $ validateTable nonsense
  where
    nonsense :: Table (Only Int)
    nonsense = table "I don't exist" [Single one :- primary]
    one = selectors nonsense

validateMultiUnique = do
    tryDropTable tbl1
    createTable tbl1
    validateTable tbl1
    assertFail $ validateTable tbl2
    dropTable tbl1

    createTable tbl2
    validateTable tbl2
    assertFail $ validateTable tbl1
    dropTable tbl2
  where
    tbl1 :: Table (Int, Int)
    tbl1 = table "foo" [(one :+ Single two) :- unique]

    tbl2 :: Table (Int, Int)
    tbl2 = table "foo" []

    (one :*: two) = selectors tbl1

validateSingleUnique = do
    tryDropTable tbl1
    createTable tbl1
    validateTable tbl1
    assertFail $ validateTable tbl2
    dropTable tbl1

    createTable tbl2
    validateTable tbl2
    assertFail $ validateTable tbl1
    dropTable tbl2
  where
    tbl1 :: Table (Int, Int)
    tbl1 = table "foo" [Single one :- unique]

    tbl2 :: Table (Int, Int)
    tbl2 = table "foo" []

    (one :*: two) = selectors tbl1

validateMultiPk = do
    tryDropTable tbl1
    createTable tbl1
    validateTable tbl1
    assertFail $ validateTable tbl2
    dropTable tbl1

    createTable tbl2
    validateTable tbl2
    assertFail $ validateTable tbl1
    dropTable tbl2
  where
    tbl1 :: Table (Int, Int)
    tbl1 = table "foo" [(one :+ Single two) :- primary]

    tbl2 :: Table (Int, Int)
    tbl2 = table "foo" []

    (one :*: two) = selectors tbl1

validateTimestamp :: SeldaM b ()
validateTimestamp = do
    tryDropTable tbl
    createTable tbl
    validateTable tbl
  where
    tbl :: Table (UTCTime, TimeOfDay)
    tbl = table "foo" []

validateAutoPrimary :: SeldaM b ()
validateAutoPrimary = do
    tryDropTable tbl
    createTable tbl
    validateTable tbl
  where
    tbl :: Table (RowID, Int)
    (tbl, one :*: two) = tableWithSelectors "blah" [one :- untypedAutoPrimary]
