-- | Foreign key support.
module Database.Selda.Table.Foreign where
import Database.Selda.Selectors
import Database.Selda.Table
import Database.Selda.Types

-- | Add a foreign key constraint to the given column, referencing
--   the column indicated by the given table and selector.
--   If the referenced column is not a primary key or has a
--   uniqueness constraint, a 'ValidationError' will be thrown
--   during validation.
fk :: ColSpec a -> (Table t, Selector t a) -> ColSpec a
fk (ColSpec [c]) (Table tn tcs, Selector i) =
    ColSpec [c {colFKs = thefk : colFKs c}]
  where
    thefk = (Table tn tcs, colName (tcs !! i))
fk _ _ =
  error "impossible: ColSpec with several columns"

