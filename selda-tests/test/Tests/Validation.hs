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
import Test.HUnit
import Utils
import Tables

validationTests freshEnv =
  [ "nul identifiers fail"               ~: freshEnv nulIdentifiersFail
  , "empty identifiers are caught"       ~: freshEnv emptyIdentifiersFail
  , "duplicate columns are caught"       ~: freshEnv duplicateColsFail
  , "duplicate PKs are caught"           ~: freshEnv duplicatePKsFail
  , "non-unique FK fails"                ~: freshEnv nonUniqueFKFails
  , "non-primary unique FK passes"       ~: freshEnv nonPrimaryUniqueFK
  , "nullable unique field passes"       ~: freshEnv nullableUnique
  , "validating wrong table fails"       ~: freshEnv validateWrongTable
  , "validating nonexistent table fails" ~: freshEnv validateNonexistentTable
  ]

nulIdentifiersFail = do
  e1 <- try (createTable nulTable) :: SeldaM (Either ValidationError ())
  e2 <- try (createTable nulColTable) :: SeldaM (Either ValidationError ())
  case (e1, e2) of
    (Left _, Left _) -> return ()
    _                -> liftIO $ assertFailure "ValidationError not thrown"
  where
    nulTable :: Table (Int, Int)
    nulTable = table "table_\0" []

    nulColTable :: Table (Int, Int)
    nulColTable = tableFieldMod "nul_col_table" [] (const "col_\0")

emptyIdentifiersFail = do
  e1 <- try (createTable noNameTable) :: SeldaM (Either ValidationError ())
  e2 <- try (createTable noColNameTable) :: SeldaM (Either ValidationError ())
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
  e <- try (createTable dupes) :: SeldaM (Either ValidationError ())
  case e of
    Left _ -> return ()
    _      -> liftIO $ assertFailure "ValidationError not thrown"
  where
    dupes :: Table (Int, Text)
    dupes = tableFieldMod "duplicate" [] (const "blah")

duplicatePKsFail = do
  e1 <- try (createTable dupes1) :: SeldaM (Either ValidationError ())
  e2 <- try (createTable dupes2) :: SeldaM (Either ValidationError ())
  case (e1, e2) of
    (Left _, Left _) -> return ()
    _                -> liftIO $ assertFailure "ValidationError not thrown"
  where
    dupes1 :: Table (Int, Text)
    dupes1 = table "duplicate" [fst :- primary, snd :- primary]
    dupes2 :: Table (RowID, Text)
    dupes2 = table "duplicate" [fst :- untypedAutoPrimary, snd :- primary]

nonUniqueFKFails = do
    res <- try (createTable addressesWithFK) :: SeldaM (Either ValidationError ())
    case res of
      Left _  -> return ()
      Right _ -> liftIO $ assertFailure "ValidationError not thrown"
  where
    addressesWithFK :: Table (Text, Text)
    addressesWithFK =
      table "addressesWithFK" [fst :- foreignKey comments cComment]

nonPrimaryUniqueFK = do
    createTable uniquePeople
    createTable addressesWithFK
    dropTable addressesWithFK
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text, Maybe Text)
    (uniquePeople, upName :*: upPet) =
      tableWithSelectors "uniquePeople" [fst :- unique]
    addressesWithFK :: Table (Text, Text)
    addressesWithFK =
      table "addressesWithFK" [fst :- foreignKey uniquePeople upName]

nullableUnique = do
    createTable uniquePeople
    dropTable uniquePeople
  where
    uniquePeople :: Table (Text, Maybe Text)
    (uniquePeople, upName :*: upPet) =
      tableWithSelectors "uniquePeople" [fst :- unique, snd :- unique]
    addressesWithFK :: Table (Text, Text)
    addressesWithFK = table "addressesWithFK" [fst :- foreignKey uniquePeople upName]

validateWrongTable = do
    assertFail $ validateTable badPeople1
    assertFail $ validateTable badPeople2
    assertFail $ validateTable badPeople3
    assertFail $ validateTable badPeople4
    assertFail $ validateTable badIxPeople1
    assertFail $ validateTable badIxPeople2
    assertFail $ validateTable badIxPeople3
    assertFail $ validateTable badIxPeople4
  where
    peopleFieldNames "col_1" = "name"
    peopleFieldNames "col_2" = "age"
    peopleFieldNames "col_3" = "pet"
    peopleFieldNames "col_4" = "cash"
    peopleFieldNames "col_5" = "extra"

    badPeople1 :: Table (Text, Int, Text, Double)
    badPeople1 = tableFieldMod "people"
      [ (\(_,_,x,_) -> x) :- index
      , (\(_,_,_,x) -> x) :- indexUsing HashIndex
      ] peopleFieldNames

    badPeople2 :: Table (Text, Bool, Maybe Text, Double)
    badPeople2 = table "people"
      [ (\(_,_,x,_) -> x) :- index
      , (\(_,_,_,x) -> x) :- indexUsing HashIndex
      ]

    badPeople3 :: Table (Text, Int, Maybe Text)
    badPeople3 = table "people"
      [ (\(_,_,x) -> x) :- index
      ]

    badPeople4 :: Table (Text, Int, Maybe Text, Double, Int)
    badPeople4 = table "people"
      [ (\(_,_,x,_,_) -> x) :- index
      , (\(_,_,_,x,_) -> x) :- indexUsing HashIndex
      ]

    badIxPeople1 :: Table (Text, Int, Maybe Text, Double)
    badIxPeople1 = table "people"
      [ (\(_,_,_,x) -> x) :- indexUsing HashIndex
      ]

    badIxPeople2 :: Table (Text, Int, Maybe Text, Double)
    badIxPeople2 = table "people"
      [ (\(_,_,x,_) -> x) :- index
      ]

    badIxPeople3 :: Table (Text, Int, Maybe Text, Double)
    badIxPeople3 = table "people"
      [ (\(x,_,_,_) -> x) :- index
      , (\(_,_,x,_) -> x) :- index
      , (\(_,_,_,x) -> x) :- indexUsing HashIndex
      ]

    badIxPeople4 :: Table (Text, Int, Maybe Text, Double)
    badIxPeople4 = table "people"
      [ (\(_,x,_,_) -> x) :- index
      , (\(_,_,x,_) -> x) :- indexUsing HashIndex
      , (\(_,_,_,x) -> x) :- indexUsing HashIndex
      ]

validateNonexistentTable = do
    assertFail $ validateTable nonsense
  where
    nonsense :: Table (Single Int)
    nonsense = table "I don't exist" [the :- primary]
