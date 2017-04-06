-- | Selda SQL compilation.
module Database.Selda.Compile where
import Database.Selda.Query.Type
import Database.Selda.SQL
import Database.Selda.SQL.Print
import Database.Selda.Column
import Database.Selda.Backend
import Database.Selda.Transform
import Data.Text (Text)
import Control.Monad.State

-- | Compile a query into a parameterised SQL statement.
compile :: Result a => Query s a -> (Text, [Param])
compile = compSql . snd . compQuery 0

-- | Compile a query to an SQL AST.
--   Groups are ignored, as they are only used by 'aggregate'.
compQuery :: Result a => Int -> Query s a -> (Int, SQL)
compQuery ns q =
    (nameSupply st, SQL final (Right [srcs]) [] [] [] Nothing)
  where
    (cs, st) = runQueryM ns q
    final = finalCols cs
    sql = state2sql st
    live = colNames $ final ++ map Some (restricts sql)
    srcs = removeDeadCols live sql
