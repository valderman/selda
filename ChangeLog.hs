-- | More structured changelog, for easy release automation.
module Main where
import Control.Monad (when)
import System.Environment
import System.Exit
import System.FilePath
import System.Process

changeLog :: ChangeLog
changeLog =
  [ Version "0.1.11.0" "2017-09-08"
    "Conditionals, SELECT DISTINCT, and bug fixes."
    [ "Fix name generation in the presence of isIn over queries."
    , "SELECT DISTINCT support."
    , "Conditional expressions and matchNull."
    ]
  , Version "0.1.10.1" "2017-08-11"
    "Code generation bug fix."
    [ "Fix name generation in the presence of multiple aggregates."
    ]
  , Version "0.1.10.0" "2017-08-01"
    "Async exception safety, bug and API fixes. Also, GHC 8.2 support."
    [ "Async exception safety."
    , "Allow MonadSelda instances not built on SeldaT."
    , "Chunk very large insertions on backends that request it (i.e. SQLite)."
    , "GHC 8.2 support."
    ]
  , Version "0.1.9.0" "2017-06-16"
    "Conditional inserts and documentation fixes."
    [ "Properly document semantics of order."
    , "Export conditional inserts."
    , "Fix Haste build for backends."
    ]
  , Version "0.1.8.0" "2017-06-10"
    "Blobs prepared statements and better backend configuration."
    [ "Move SQL pretty-printing config into a single type."
    , "Support for binary blobs."
    , "Support for prepared statements."
    , "Support for connection reuse across Selda computations."
    , "Cleaner and more robust backend API."
    , "Stricter type constraints on comparisons."
    , "Allow limit on inner queries."
    , "Allow inspecting row identifiers."
    ]
  , Version "0.1.7.0" "2017-05-17"
    "More upserts and abstracting over auto-incrementing primary keys."
    [ "Add specialized insertUnless upsert variant."
    , "Fix potential race condition in upserts."
    , "Use abstract row identifier type for auto-incrementing primary keys."
    , "Less strict version bounds on dependencies."
    ]

  , Version "0.1.6.0" "2017-05-07"
    "More expressivity for inserts and queries."
    [ "Conditional insert (\"upsert\") support."
    , "Support `SELECT x IN (SELECT ...)` and `SELECT x IN (a, b, ...)` queries."
    , "Explicit inner queries."
    , "Rename `inner` to `innerJoin`, more intuitive behavior for `suchThat`."
    , "Add `from` shorthand for `\\s q -> fmap (!s) q`."
    , "Unique and foreign key constraints for generics."
    ]

  , Version "0.1.5.0" "2017-05-05"
    "Inner joins and bug fixing for round_ and cast."
    [ "Inner join support."
    , "More sensible names in backend API."
    , "Fix rounding and casts."
    ]

  , Version "0.1.4.1" "2017-05-04"
    "Cache consistency bugfix release."
    [ "Fix cache consistency bug in the presence of multiple databases."
    ]

  , Version "0.1.4.0" "2017-05-04"
    "Foreign keys, unique constraints, and less postgres NOTICE spam."
    [ "Add uniqueness constraints and foreign keys."
    ]

  , Version "0.1.3.3" "2017-05-04"
    "Bug fix for caching with transactions; documentation fixes."
    [ "Fix cache invalidation race when using transactions."
    ]

  , Version "0.1.3.2" "2017-05-01"
    "Discipline and document error handling."
    [ "Only throw well-documented, Selda-specific exceptions."
    ]

  , Version "0.1.3.1" "2017-05-01"
    "README fixes to appease Hackage."
    [ "More Hackage-friendly README."
    ]

  , Version "0.1.3.0" "2017-04-30"
    "Selectors, bug fixes and minor API improvements."
    [ "Add selectors for non-generic tables."
    , "Allow default insertions on all columns."
    , "More sensible API for LIMIT."
    , "Fix broken SQL being generated for pathological corner cases."
    , "Documentation fixes."
    ]

  , Version "0.1.2.0" "2017-04-20"
    "Replace ¤ with :*: for table definitions."
    [ "Replace `¤` with `:*:` in table definitions."
    ]

  , Version "0.1.1.1" "2017-04-20"
    "Minor documentation fixes."
    [ "Minor documentation fixes."
    ]

  , Version "0.1.1.0" "2017-04-20"
    "Generics, SELECT FROM VALUES, plus minor fixes."
    [ "Generic tables, queries and mutation."
    , "Select from inline tables."
    , "Tutorial updates."
    , "Minor bugfixes."
    ]

  , Version "0.1.0.0" "2017-04-14"
    "Initial release."
    [ "Initial release."
    ]
  ]

data Version = Version
  { vVersion :: String
  , vDate    :: String
  , vSummary :: String
  , vChanges :: [String]
  }

type ChangeLog = [Version]

-- | Get the current version.
currentVersion :: [Version] -> String
currentVersion = vVersion . head

-- | Get the release date of the current version.
currentDate :: [Version] -> String
currentDate = vDate . head

-- | Get the release summary of the current version.
currentSummary :: [Version] -> String
currentSummary = vDate . head

-- | Get the git tag message for the current version, as a list of @-m <msg>@
--   flags.
gitTagFlags :: [Version] -> [String]
gitTagFlags (v:_) =
  ["-m", vSummary v] ++ concat [["-m", c] | c <- vChanges v]

-- | Convert a changelog to a markdown file suitable for uploading to Hackage.
toMD :: String -> ChangeLog -> String
toMD pkgname vs = init $ init $ unlines (heading : "\n" : map verToMD vs)
  where
    heading = "# Revision history for " ++ pkgname
    verToMD v = unlines
      [ "## " ++ vVersion v ++ " -- " ++ vDate v
      , ""
      , unlines ["* " ++ ch | ch <- vChanges v]
      ]

-- | @git tag@ the current version. Fails if the tag already exists.
tagCurrentVersion :: ChangeLog -> IO ()
tagCurrentVersion ch = callProcess "git" flags
  where
    flags = concat
      [ ["tag", "-a", currentVersion ch]
      , gitTagFlags ch
      ]

-- | Write the changelog to @ChangeLog.md@ in the given directory.
writeChangelogMD :: String -> FilePath -> ChangeLog -> IO ()
writeChangelogMD pkgname dir = writeFile (dir </> "ChangeLog.md") . toMD pkgname

main = do
  args <- getArgs
  when (null args || any (not . (`elem` ["tag", "md"])) args) $ do
    putStrLn "usage: runghc ChangeLog.hs tag|md"
    exitFailure
  when ("tag" `elem` args) $ tagCurrentVersion changeLog
  when ("md" `elem` args) $ writeChangelogMD "Selda" "selda" changeLog
