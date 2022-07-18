-- | Functionality for inspecting and debugging Selda queries.
module Database.Selda.Debug
  ( OnError (..), defPPConfig
  , compile
  , compileCreateTable, compileDropTable
  , compileInsert, compileUpdate
  ) where
import Database.Selda.SQL.Print.Config ( defPPConfig )
import Database.Selda.Compile
    ( compile, compileInsert, compileUpdate )
import Database.Selda.Table.Compile
    ( OnError(..), compileCreateTable, compileDropTable )
