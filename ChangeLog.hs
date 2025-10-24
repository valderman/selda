-- | More structured changelog, for easy release automation.
module Main where
import Control.Monad (when)
import Data.Time
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Read

changeLog :: ChangeLog
changeLog =
  [ Version "0.5.3.0" "2025-10-24"
    "Streaming support and maintenance"
    [ "Add support streaming of large results (#200)"
    , "Publish the website via GitHub pages (#201)"
    , "Properly use cascading deletes with foreign keys (#191)"
    , "Schema query validation fix for PostgreSQL (#183)"
    , "Many small version compatibility fixes"
    ]
  , Version "0.5.2.0" "2022-09-18"
    "Quality of life improvements"
    [ "Add support for GHC versions 8.10-9.2"
    , "Add typed UUIDs"
    , "Allow literal rows in update queries. (#139)"
    , "Add support for UNION/UNION ALL. (#140)"
    , "Support raw PostgreSQL connetion strings. (#136)"
    , "Move to Docker and GitHub actions for testing."
    , "Drop support for GHC versions <8.8."
    , "Various bugfixes."
    ]
  , Version "0.5.1.0" "2020-01-20"
    "Raw SQL support"
    [ "Support for raw SQL fragments. (#134)"
    , "Expose tableName."
    , "Document performance drawbacks of withoutForeignKeyEnforcement."
    , "Fix several bugs validating auto-incrementing PKs. (#133)"
    ]
  , Version "0.5.0.0" "2019-09-21"
    "Multi-column indexes and better type errors."
    [ "index and indexUsing now accept a Group instead of a Selector (#121)"
    , "Custom type errors for scope mismatches."
    , "Provide Generic instances for ID and RowID."
    , "Provide To/FromJSON instances for ID and RowID (selda-json)."
    , "Add back MonadTrans instance for SeldaT."
    ]
  , Version "0.4.0.0" "2019-06-02"
    "Major update."
    [ "Type-safe support for backend-specific functionality. Top level query definitions now require explicit type signature. (#80)"
    , "Native UUID support. (#47)"
    , "Support JSON columns on all backends through aeson."
    , "Support JSON lookups (i.e. SELECT json_column.some_property FROM ...) on PostgreSQL."
    , "Multi-column primary key and uniqueness constraint support. (#25, #99)"
    , "Switch to PostgreSQL binary protocol. (#18)"
    , "Prevent dangerous user-defined SQL result instances."
    , "Expose backend internals through Database.Selda.Backend.Internal. (#109)"
    , "Expose SQLite connection handle. (#101)"
    , "Make MonadSelda more amenable to connection pooling. (#108)"
    , "Add weakly auto-incrementing primary keys. (#94)"
    , "Move compile* functions to Database.Selda.Debug."
    , "Remove half the tuple convenience functions."
    , "Remove in-process cache. (#117)"
    , "Officially support GHC 8.6, 8.8 (SQLite only until postgres dependencies catch up with 8.8)."
    , "Drop support for GHC 7.10. (#118)"
    , "Manual (i.e. non record label) selectors are no longer exported by default; import Database.Selda.MakeSelectors is you need them. (#118)"
    , "Update toolchain to use v2-style cabal commands."
    , "Fix date/time types for PostgreSQL. (#104)"
    , "Fix bug when migrating tables with indexes. (#107)"
    , "Misc. smaller bug fixes."
    ]
  , Version "0.3.4.0" "2018-09-29"
    "Nullable convenience functions."
    [ "Added convenience functions for working with nullable columns."
    ]
  , Version "0.3.3.1" "2018-09-04"
    "Fixing DISTINCT."
    [ "DISTINCT should now always return distinct results."
    , "DISTINCT can no longer produce ill-scoped queries."
    ]
  , Version "0.3.3.0" "2018-09-01"
    "Ad hoc selectors on GHC 8.0 and up."
    [ "Ad hoc selectors using OverloadedLabels."
    , "Shorter build times."
    , "Minor API updates and simplifications."
    ]
  , Version "0.3.2.0" "2018-08-07"
    "Minor API improvements and bug fixes."
    [ "Some aggregates are now nullable."
    , "sum_ on an empty table doesn't crash anymore."
    , "Aggregating over an empty selectValues doesn't crash anymore."
    ]
  , Version "0.3.1.0" "2018-08-06"
    "Minor API fix when defining table attributes."
    [ "Minor API fix when defining table attributes." ]
  , Version "0.3.0.0" "2018-08-05"
    "Migrations, indexes, validation, and major API overhaul."
    [ "Support for Stack and GHC 8.4."
    , "Precedence fix for selector index (!) operator."
    , "Accept INT and SMALLINT columns in user-created PostgreSQL tables."
    , "Add combinator for turning off foreign key checking."
    , "Rename unsafeRowId/unsafeId to toRowId/rowId."
    , "Add typed row identifiers."
    , "More generic type for sum_."
    , "Table validation against current database."
    , "Basic migration support."
    , "Basic index support."
    , "Remove ad hoc tables; only generic tables from now on."
    ]
  , Version "0.2.0.0" "2018-04-02"
    "Fixes for inner queries and generic tables."
    [ "Support custom column names for generic tables."
    , "Scope safety fix for inner queries."
    , "Better type errors on GHC 8+ for inner queries."
    ]
  , Version "0.1.12.1" "2018-02-27"
    "Backend API fixes for PostgreSQL."
    [ "New PPConfig hook for more flexibility when compiling types."
    ]
  , Version "0.1.12.0" "2018-01-11"
    "Recursive foreign keys, arbitrary enums, and bug fixes."
    [ "Allow recursive and optional foreign keys."
    , "Allow arbitrary enums in tables, represented as text."
    , "Fix RowID issues for PostgreSQL."
    , "Fix auto-incrementing primary keys for generic tables."
    ]
  , Version "0.1.11.2" "2017-12-14"
    "PostgreSQL bugfix release."
    [ "Fix treatment of booleans in PostgreSQL backend."
    ]
  , Version "0.1.11.1" "2017-10-10"
    "Fix a rare but serious cache bug."
    [ "Fix rare infinite loop bug in in-process cache."
    ]
  , Version "0.1.11.0" "2017-09-08"
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
  { vVersion :: String -- ^ X.Y.Z.W
  , vDate    :: String -- ^ YYYY-MM-DD
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

-- | Get the latest entry in the changelog.
latestEntry :: ChangeLog -> Version
latestEntry = head

-- | Get the release date from the latest entry in the changelog.
latestReleaseDate :: ChangeLog -> String
latestReleaseDate = vDate . latestEntry

-- | Get the version number from the latest entry in the changelog.
latestVersion :: ChangeLog -> String
latestVersion = vVersion . latestEntry

-- | Does the given string contain a valid release date?
validReleaseDate :: String -> Bool
validReleaseDate date = and
    [ validate (\y -> y >= 2017 && y <  2100) year
    , validate (\m -> m >= 1    && m <= 12) month
    , validate (\d -> d >= 1    && d <= 31) day
    ]
  where
    year  = take 4 date
    month = take 2 $ drop 5 date
    day   = drop 8 date

-- | Is the given string a valid version number?
validVersion :: String -> Bool
validVersion version =
    all (validate (\x -> x >= 0 && x <= 99)) parts && length parts == 4
  where
    parts = words $ map undot version
    undot '.' = ' '
    undot c   = c

-- | Returns @True@ if the given string is readable at the given type, and the
--   resulting value fulfills the given predicate.
validate :: Read a => (a -> Bool) -> String -> Bool
validate p s =
  case readMaybe s of
    Just x -> p x
    _      -> False

-- | Ensure that the changelog is valid for release.
validateChangeLog :: ChangeLog -> IO ()
validateChangeLog changelog = do
    let date = latestReleaseDate changeLog
        version = latestVersion changeLog

    when (not $ validReleaseDate date) $ do
      putStrLn ("Invalid release date: " ++ date)
      exitFailure

    when (not $ validVersion version) $ do
      putStrLn ("Invalid version: " ++ version)
      exitFailure

    today <- utctDay <$> getCurrentTime
    when (show today /= date) $ do
      putStrLn "Release date does not match today's date."
      exitFailure

    when (null $ vSummary $ latestEntry changeLog) $ do
      putStrLn "Changelog has no summary."
      exitFailure

    when (null $ vChanges $ latestEntry changeLog) $ do
      putStrLn "Changelog has no list of changes."
      exitFailure

    when (head version /= '0') $ do
      putStrLn "Major version is not 0; make upload will complain."
      exitFailure

main = do
  args <- getArgs
  when (null args || any (not . (`elem` ["tag", "md", "validate"])) args) $ do
    putStrLn "usage: runghc ChangeLog.hs tag|md|validate"
    exitFailure

  when ("validate" `elem` args) $ do
    validateChangeLog changeLog
  when ("tag" `elem` args) $ do
    tagCurrentVersion changeLog
  when ("md" `elem` args) $ do
    writeChangelogMD "Selda" "selda" changeLog
