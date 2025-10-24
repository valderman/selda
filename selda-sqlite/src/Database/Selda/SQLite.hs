{-# LANGUAGE GADTs, CPP, OverloadedStrings #-}
-- | SQLite3 backend for Selda.
module Database.Selda.SQLite
  ( SQLite
  , withSQLite
  , sqliteOpen, seldaClose
  , sqliteBackend
  ) where
import Database.Selda
import Database.Selda.Backend
import Database.Selda.SQLite.Parser
import Data.Maybe (fromJust)
#ifndef __HASTE__
import Control.Monad (void, when, unless)
import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic
import Data.Int (Int64)
import Data.Text as Text (pack, toLower, take)
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
import Data.UUID.Types (toByteString)
import Database.SQLite3
import System.Directory (makeAbsolute)
#endif

data SQLite

-- | Open a new connection to an SQLite database.
--   The connection is reusable across calls to `runSeldaT`, and must be
--   explicitly closed using 'seldaClose' when no longer needed.
sqliteOpen :: (MonadIO m, MonadMask m) => FilePath -> m (SeldaConnection SQLite)
#ifdef __HASTE__
sqliteOpen _ = error "sqliteOpen called in JS context"
#else
sqliteOpen file =
    bracketOnError acquire (liftIO . close) $ \db -> do
      absFile <- liftIO $ pack <$> makeAbsolute file
      let backend = sqliteBackend db
      void . liftIO $ runStmt backend "PRAGMA foreign_keys = ON;" []
      newConnection backend absFile
  where
    acquire = do
      edb <- try $ liftIO $ open (pack file)
      case edb of
        Left e@(SQLError{}) -> throwM (DbError (show e))
        Right db -> pure db
#endif

-- | Perform the given computation over an SQLite database.
--   The database is guaranteed to be closed when the computation terminates.
withSQLite :: (MonadIO m, MonadMask m) => FilePath -> SeldaT SQLite m a -> m a
#ifdef __HASTE__
withSQLite _ _ = return $ error "withSQLite called in JS context"

sqliteBackend :: a -> SeldaBackend
sqliteBackend _ = error "sqliteBackend called in JS context"
#else
withSQLite file m = bracket (sqliteOpen file) seldaClose (runSeldaT m)

-- | Create a Selda backend using an already open database handle.
--   This is useful for situations where you want to use some SQLite-specific
--   functionality alongside Selda.
--
--   Note that manipulating the underlying database handle directly voids
--   any and all safety guarantees made by the Selda API.
--   Caching functionality in particular WILL break.
--   Proceed with extreme caution.
sqliteBackend :: Database -> SeldaBackend SQLite
sqliteBackend db = SeldaBackend
  { runStmt         = \q ps -> snd <$> sqliteQueryRunner db q ps
  , runStmtWithPK   = \q ps -> fst <$> sqliteQueryRunner db q ps
  , prepareStmt     = \_ _ -> sqlitePrepare db
  , runPrepared     = sqliteRunPrepared db
  , getTableInfo    = sqliteGetTableInfo db . fromTableName
  , ppConfig        = defPPConfig {ppMaxInsertParams = Just 999}
  , backendId       = SQLite
  , closeConnection = \conn -> do
      stmts <- allStmts conn
      flip mapM_ stmts $ \(_, stm) -> do
        finalize $ fromDyn stm (error "BUG: non-statement SQLite statement")
      close db
  , disableForeignKeys = disableFKs db
  }

sqliteGetTableInfo :: Database -> Text -> IO TableInfo
sqliteGetTableInfo db tbl = do
    cols <- (snd . snd) <$> sqliteQueryRunner db tblinfo []
    fks <- (snd . snd) <$> sqliteQueryRunner db fklist []
    createQuery <- (snd . snd) <$> sqliteQueryRunner db autos []
    let cs = case createQuery of
          [[SqlString q]] -> colsFromQuery q
          _               -> []
    ixs <- mapM indexInfo . snd . snd =<< sqliteQueryRunner db indexes []
    colInfos <- mapM (describe fks ixs cs) cols
    return $ TableInfo
      { tableInfoName = mkTableName tbl
      , tableColumnInfos = colInfos
      , tableUniqueGroups =
        [ map mkColName names
        | (names, "u") <- ixs
        ]
      , tablePrimaryKey = concat
        [ concat
          [ map mkColName names
          | (names, "pk") <- ixs
          ]
        , [ colName ci
          | ci <- colInfos
          , colIsAutoPrimary ci
          ]
        ]
      }
  where
    tblinfo = mconcat ["PRAGMA table_info(", tbl, ");"]
    indexes = mconcat ["PRAGMA index_list(", tbl, ");"]
    fklist = mconcat ["PRAGMA foreign_key_list(", tbl, ");"]
    autos = mconcat ["SELECT sql FROM sqlite_master WHERE name = ", tbl, ";"]
    ixinfo name = mconcat ["PRAGMA index_info(", name, ");"]

    toTypeRep _ "text"                      = Right TText
    toTypeRep _ "double precision"          = Right TFloat
    toTypeRep _ "double"                    = Right TFloat
    toTypeRep _ "boolean"                   = Right TBool
    toTypeRep _ "datetime"                  = Right TDateTime
    toTypeRep _ "date"                      = Right TDate
    toTypeRep _ "time"                      = Right TTime
    toTypeRep _ "blob"                      = Right TBlob
    toTypeRep True "integer"                = Right TRowID
    toTypeRep pk s | Text.take 6 s == "bigint" = Right $ if pk then TRowID else TInt64
    toTypeRep pk s | Text.take 3 s == "int" = Right $ if pk then TRowID else TInt32
    toTypeRep _ typ                         = Left typ

    indexInfo [_, SqlString ixname, _, SqlString itype, _] = do
      let q = ixinfo ixname
      info <- (snd . snd) <$> sqliteQueryRunner db q []
      return $ (map (\[_,_,SqlString name] -> name) info, itype)
    indexInfo _ = do
      error "unreachable"

    describe fks ixs cs [_, SqlString name, SqlString ty, SqlInt64 nonnull, _, SqlInt64 pk] = do
      let ty' = Text.toLower ty
      return $ ColumnInfo
        { colName = mkColName name
        , colType = toTypeRep (pk == 1) ty'
        , colIsAutoPrimary = snd $ fromJust $ lookup name cs
        , colHasIndex = any (== ([name], "c")) ixs
        , colIsNullable = nonnull == 0
        , colFKs =
            [ (mkTableName reftbl, mkColName refkey)
            | (_:_:SqlString reftbl:SqlString key:SqlString refkey:_) <- fks
            , key == name
            ]
        }
    describe _ _ _ result = do
      throwM $ SqlError $ "bad result from PRAGMA table_info: " ++ show result

disableFKs :: Database -> Bool -> IO ()
disableFKs db disable = do
    unless disable $ void $ sqliteQueryRunner db "COMMIT;" []
    void $ sqliteQueryRunner db q []
    when disable $ void $ sqliteQueryRunner db "BEGIN TRANSACTION;" []
  where
    q | disable   = "PRAGMA foreign_keys = OFF;"
      | otherwise = "PRAGMA foreign_keys = ON;"

sqlitePrepare :: Database -> Text -> IO Dynamic
sqlitePrepare db qry = do
  eres <- try $ prepare db qry
  case eres of
    Left e@(SQLError{}) -> throwM (SqlError (show e))
    Right r             -> return $ toDyn r

sqliteRunPrepared :: Database -> Dynamic -> [Param] -> IO (Int, [[SqlValue]])
sqliteRunPrepared db hdl params = do
  eres <- try $ do
    let Just stm = fromDynamic hdl
    sqliteRunStmt db stm params `finally` do
      clearBindings stm
      reset stm
  case eres of
    Left e@(SQLError{}) -> throwM (SqlError (show e))
    Right res           -> return (snd res)

sqliteQueryRunner :: Database -> QueryRunner (Int64, (Int, [[SqlValue]]))
sqliteQueryRunner db qry params = do
    eres <- try $ do
      stm <- prepare db qry
      sqliteRunStmt db stm params `finally` do
        finalize stm
    case eres of
      Left e@(SQLError{}) -> throwM (SqlError (show e))
      Right res           -> return res

sqliteRunStmt :: Database -> Statement -> [Param] -> IO (Int64, (Int, [[SqlValue]]))
sqliteRunStmt db stm params = do
  bind stm [toSqlData p | Param p <- params]
  rows <- getRows stm []
  rid <- lastInsertRowId db
  cs <- changes db
  return (fromIntegral rid, (cs, [map fromSqlData r | r <- rows]))

getRows :: Statement -> [[SQLData]] -> IO [[SQLData]]
getRows s acc = do
  res <- step s
  case res of
    Row -> do
      cs <- columns s
      getRows s (cs : acc)
    _ -> do
      return $ reverse acc

toSqlData :: Lit a -> SQLData
toSqlData (LInt32 i)    = SQLInteger $ fromIntegral i
toSqlData (LInt64 i)    = SQLInteger $ fromIntegral i
toSqlData (LDouble d)   = SQLFloat d
toSqlData (LText s)     = SQLText s
toSqlData (LDateTime t) = SQLText $ pack $ fmtTime sqlDateTimeFormat t
toSqlData (LDate d)     = SQLText $ pack $ fmtTime sqlDateFormat d
toSqlData (LTime t)     = SQLText $ pack $ fmtTime sqlTimeFormat t
toSqlData (LBool b)     = SQLInteger $ if b then 1 else 0
toSqlData (LBlob b)     = SQLBlob b
toSqlData (LNull)       = SQLNull
toSqlData (LJust x)     = toSqlData x
toSqlData (LCustom _ l) = toSqlData l
toSqlData (LUUID x)     = SQLBlob (toStrict $ toByteString x)

fromSqlData :: SQLData -> SqlValue
fromSqlData (SQLInteger i) = SqlInt64 $ fromIntegral i
fromSqlData (SQLFloat f)   = SqlFloat f
fromSqlData (SQLText s)    = SqlString s
fromSqlData (SQLBlob b)    = SqlBlob b
fromSqlData SQLNull        = SqlNull

fmtTime :: FormatTime t => String -> t -> String
fmtTime = formatTime defaultTimeLocale
#endif
