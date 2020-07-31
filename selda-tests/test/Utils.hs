{-# LANGUAGE FlexibleContexts #-}
-- | Utility functions that are useful for all tests.
module Utils where
import Control.Monad.Catch
import Data.Text (unpack)
import Database.Selda
import Database.Selda.Debug (compile)
import Test.HUnit

-- | Assert that the given computation should fail.
assertFail :: SeldaM b a -> SeldaM b ()
assertFail m = do
  res <- try m
  case res of
    Left (SomeException _) -> return ()
    _                      -> liftIO $ assertFailure "computation did not fail"

-- | @SeldaT@ wrapper for 'assertEqual'.
assEq :: (Show a, Eq a) => String -> a -> a -> SeldaM b ()
assEq s expect actual = liftIO $ assertEqual s expect actual

-- | @SeldaT@ wrapper for 'assertEqual'.
assQueryEq :: (Result a, Show (Res a), Eq (Res a)) => String -> [Res a] -> Query b a -> SeldaM b ()
assQueryEq s expect q = do
  eactual <- try $! query q >>= mapM (\x -> pure $! x)
  let msg = "Generated query:\n" ++ unpack (fst $ compile q) ++ "\n"
  case eactual of
    Right actual ->
      liftIO $ assertEqual (s ++ "\n" ++ msg) expect actual
    Left (SomeException e) ->
      ass (msg ++ "\nException thrown:\n" ++ show e ++ "\n") False

-- | @SeldaT@ wrapper for 'assertBool'.
ass :: String -> Bool -> SeldaM b ()
ass s pred = liftIO $ assertBool s pred
