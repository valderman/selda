{-# LANGUAGE TypeOperators, OverloadedStrings #-}
-- | Schema validation tests.
module Tests.Validation (validationTests) where
import Control.Concurrent
import Control.Monad.Catch
import Data.List hiding (groupBy, insert)
import Data.Time
import Database.Selda
import Database.Selda.Validation
import Database.Selda.Backend
import Database.Selda.Generic
import Test.HUnit
import Utils
import Tables

validationTests freshEnv =
  [ "nul identifiers fail"           ~: freshEnv nulIdentifiersFail
  , "empty identifiers are caught"   ~: freshEnv emptyIdentifiersFail
  , "duplicate columns are caught"   ~: freshEnv duplicateColsFail
  , "duplicate PKs are caught"       ~: freshEnv duplicatePKsFail
  , "non-unique FK fails"            ~: freshEnv nonUniqueFKFails
  , "non-primary unique FK passes"   ~: freshEnv nonPrimaryUniqueFK
  , "nullable unique field passes"   ~: freshEnv nullableUnique
  , "validating wrong table fails"   ~: freshEnv validateWrongTable
  ]

nulIdentifiersFail = do
  e1 <- try (createTable nulTable) :: SeldaM (Either ValidationError ())
  e2 <- try (createTable nulColTable) :: SeldaM (Either ValidationError ())
  case (e1, e2) of
    (Left _, Left _) -> return ()
    _                -> liftIO $ assertFailure "ValidationError not thrown"
  where
    nulTable :: Table Int
    nulTable = table "table_\0" $ required "blah"

    nulColTable :: Table Int
    nulColTable = table "nul_col_table" $ required "col_\0"

emptyIdentifiersFail = do
  e1 <- try (createTable noNameTable) :: SeldaM (Either ValidationError ())
  e2 <- try (createTable noColNameTable) :: SeldaM (Either ValidationError ())
  case (e1, e2) of
    (Left _, Left _) -> return ()
    _                -> liftIO $ assertFailure "ValidationError not thrown"
  where
    noNameTable :: Table Int
    noNameTable = table "" $ required "blah"

    noColNameTable :: Table Int
    noColNameTable = table "table with empty col name" $ required ""

duplicateColsFail = do
  e <- try (createTable dupes) :: SeldaM (Either ValidationError ())
  case e of
    Left _ -> return ()
    _      -> liftIO $ assertFailure "ValidationError not thrown"
  where
    dupes :: Table (Int :*: Text)
    dupes = table "duplicate" $ required "blah" :*: required "blah"

duplicatePKsFail = do
  e1 <- try (createTable dupes1) :: SeldaM (Either ValidationError ())
  e2 <- try (createTable dupes2) :: SeldaM (Either ValidationError ())
  case (e1, e2) of
    (Left _, Left _) -> return ()
    _                -> liftIO $ assertFailure "ValidationError not thrown"
  where
    dupes1 :: Table (Int :*: Text)
    dupes1 = table "duplicate" $ primary "blah1" :*: primary "blah2"
    dupes2 :: Table (RowID :*: Text)
    dupes2 = table "duplicate" $ autoPrimary "blah1" :*: primary "blah2"

nonUniqueFKFails = do
    res <- try (createTable addressesWithFK) :: SeldaM (Either ValidationError ())
    case res of
      Left _  -> return ()
      Right _ -> liftIO $ assertFailure "ValidationError not thrown"
  where
    addressesWithFK :: Table (Text :*: Text)
    addressesWithFK =
          table "addressesWithFK"
      $   required "name" `fk` (comments, cComment)
      :*: required "city"

nonPrimaryUniqueFK = do
    createTable uniquePeople
    createTable addressesWithFK
    dropTable addressesWithFK
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text :*: Maybe Text)
    (uniquePeople, upName :*: upPet) =
          tableWithSelectors "uniquePeople"
      $   unique (required "name")
      :*: optional "pet"
    addressesWithFK :: Table (Text :*: Text)
    addressesWithFK =
          table "addressesWithFK"
      $   required "name" `fk` (uniquePeople, upName)
      :*: required "city"

nullableUnique = do
    createTable uniquePeople
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text :*: Maybe Text)
    (uniquePeople, upName :*: upPet) =
          tableWithSelectors "uniquePeople"
      $   unique (required "name")
      :*: unique (optional "pet")
    addressesWithFK :: Table (Text :*: Text)
    addressesWithFK =
          table "addressesWithFK"
      $   required "name" `fk` (uniquePeople, upName)
      :*: required "city"

validateWrongTable = do
    assertFail $ validateTable (gen badGenPeople)
    assertFail $ validateTable badPeople1
    assertFail $ validateTable badPeople2
    assertFail $ validateTable badPeople3
    assertFail $ validateTable badPeople4
  where
    badGenPeople :: GenTable Person
    badGenPeople = genTable "genpeople" [name :- uniqueGen]

    badPeople1 :: Table (Text :*: Int :*: Text :*: Double)
    badPeople1 =
          table "people"
      $   primary "name"
      :*: required "age"
      :*: required "pet"
      :*: required "cash"

    badPeople2 :: Table (Text :*: Bool :*: Maybe Text :*: Double)
    badPeople2 =
          table "people"
      $   primary "name"
      :*: required "age"
      :*: optional "pet"
      :*: required "cash"

    badPeople3 :: Table (Text :*: Bool :*: Maybe Text)
    badPeople3 =
          table "people"
      $   primary "name"
      :*: required "age"
      :*: optional "pet"

    badPeople4 :: Table (Text :*: Bool :*: Maybe Text :*: Double :*: Int)
    badPeople4 =
          table "people"
      $   primary "name"
      :*: required "age"
      :*: optional "pet"
      :*: required "cash"
      :*: required "extra"
