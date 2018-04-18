{-# LANGUAGE OverloadedStrings #-}
-- | Foreign key support.
module Database.Selda.Table.Foreign where
import Database.Selda.Selectors
import Database.Selda.Table
import Unsafe.Coerce

-- | Add a foreign key constraint to the given column, referencing
--   the column indicated by the given table and selector.
--   If the referenced column is not a primary key or has a
--   uniqueness constraint, a 'ValidationError' will be thrown
--   during validation.
fk :: ColSpec a -> (Table t, Selector t a) -> ColSpec a
fk (ColSpec [c]) (tbl, Selector i) =
    ColSpec [c {colFKs = thefk : colFKs c}]
  where
    Table _ tcs _ _ = tbl
    thefk = (unsafeCoerce tbl, colName (tcs !! i))
fk _ _ =
  error "impossible: ColSpec with several columns"

-- | Like 'fk', but for nullable foreign keys.
optFk :: ColSpec (Maybe a) -> (Table t, Selector t a) -> ColSpec (Maybe a)
optFk (ColSpec [c]) (tbl, Selector i) =
    ColSpec [c {colFKs = thefk : colFKs c}]
  where
    Table _ tcs _ _ = tbl
    thefk = (unsafeCoerce tbl, colName (tcs !! i))
optFk _ _ =
  error "impossible: ColSpec with several columns"
