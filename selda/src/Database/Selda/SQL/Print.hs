{-# LANGUAGE GADTs, OverloadedStrings #-}
-- | Pretty-printing for SQL queries. For some values of pretty.
module Database.Selda.SQL.Print where
import Database.Selda.Column
import Database.Selda.SQL
import Database.Selda.SQL.Print.Config (PPConfig)
import qualified Database.Selda.SQL.Print.Config as Cfg
import Database.Selda.SqlType
import Database.Selda.Types
import Control.Monad.State
import Data.List
import Data.Monoid hiding (Product)
import Data.Text (Text)
import qualified Data.Text as Text

-- | O(n log n) equivalent of @nub . sort@
snub :: (Ord a, Eq a) => [a] -> [a]
snub = map head . group . sort

-- | SQL pretty-printer. The state is the list of SQL parameters to the
--   prepared statement.
type PP = State PPState

data PPState = PPState
  { ppParams  :: ![Param]
  , ppTables  :: ![TableName]
  , ppParamNS :: !Int
  , ppQueryNS :: !Int
  , ppConfig  :: !PPConfig
  }

-- | Run a pretty-printer.
runPP :: PPConfig
      -> PP Text
      -> ([TableName], (Text, [Param]))
runPP cfg pp =
  case runState pp (PPState [] [] 1 0 cfg) of
    (q, st) -> (snub $ ppTables st, (q, reverse (ppParams st)))

-- | Compile an SQL AST into a parameterized SQL query.
compSql :: PPConfig
        -> SQL
        -> ([TableName], (Text, [Param]))
compSql cfg = runPP cfg . ppSql

-- | Compile a single column expression.
compExp :: PPConfig -> Exp SQL a -> (Text, [Param])
compExp cfg = snd . runPP cfg . ppCol

-- | Compile an @UPATE@ statement.
compUpdate :: PPConfig
           -> TableName
           -> Exp SQL Bool
           -> [(ColName, SomeCol SQL)]
           -> (Text, [Param])
compUpdate cfg tbl p cs = snd $ runPP cfg ppUpd
  where
    ppUpd = do
      updates <- mapM ppUpdate cs
      check <- ppCol p
      pure $ Text.unwords
        [ "UPDATE", fromTableName tbl
        , "SET", set updates
        , "WHERE", check
        ]
    ppUpdate (n, c) = do
      let n' = fromColName n
      c' <- ppSomeCol c
      let upd = Text.unwords [n', "=", c']
      if n' == c'
        then pure $ Left upd
        else pure $ Right upd
    -- if the update doesn't change anything, pick an arbitrary column to
    -- set to itself just to satisfy SQL's syntactic rules
    set us =
      case [u | Right u <- us] of
        []  -> set (take 1 [Right u | Left u <- us])
        us' -> Text.intercalate ", " us'

-- | Compile a @DELETE@ statement.
compDelete :: PPConfig -> TableName -> Exp SQL Bool -> (Text, [Param])
compDelete cfg tbl p = snd $ runPP cfg ppDelete
  where
    ppDelete = do
      c' <- ppCol p
      pure $ Text.unwords ["DELETE FROM", fromTableName tbl, "WHERE", c']

-- | Pretty-print a literal as a named parameter and save the
--   name-value binding in the environment.
ppLit :: Lit a -> PP Text
ppLit LNull     = pure "NULL"
ppLit (LJust l) = ppLit l
ppLit l         = do
  PPState ps ts ns qns tr <- get
  put $ PPState (Param l : ps) ts (succ ns) qns tr
  return $ Text.pack ('$':show ns)

dependOn :: TableName -> PP ()
dependOn t = do
  PPState ps ts ns qns tr <- get
  put $ PPState ps (t:ts) ns qns tr

-- | Generate a unique name for a subquery.
freshQueryName :: PP Text
freshQueryName = do
  PPState ps ts ns qns tr <- get
  put $ PPState ps ts ns (succ qns) tr
  return $ Text.pack ('q':show qns)

-- | Pretty-print an SQL AST.
ppSql :: SQL -> PP Text
ppSql (SQL cs src r gs ord lim dist) = do
  cs' <- mapM ppSomeCol cs
  src' <- ppSrc src
  r' <- ppRestricts r
  gs' <- ppGroups gs
  ord' <- ppOrder ord
  lim' <- ppLimit lim
  pure $ mconcat
    [ "SELECT ", if dist then "DISTINCT " else "", result cs'
    , src'
    , r'
    , gs'
    , ord'
    , lim'
    ]
  where
    result []  = "1"
    result cs' = Text.intercalate ", " cs'

    ppSrc EmptyTable = do
      qn <- freshQueryName
      pure $ " FROM (SELECT NULL LIMIT 0) AS " <> qn
    ppSrc (TableName n)  = do
      dependOn n
      pure $ " FROM " <> fromTableName n
    ppSrc (Product [])   = do
      pure ""
    ppSrc (Product sqls) = do
      srcs <- mapM ppSql (reverse sqls)
      qs <- flip mapM ["(" <> s <> ")" | s <- srcs] $ \q -> do
        qn <- freshQueryName
        pure (q <> " AS " <> qn)
      pure $ " FROM " <> Text.intercalate ", " qs
    ppSrc (Values row rows) = do
      row' <- Text.intercalate ", " <$> mapM ppSomeCol row
      rows' <- mapM ppRow rows
      qn <- freshQueryName
      pure $ mconcat
        [ " FROM (SELECT "
        , Text.intercalate " UNION ALL SELECT " (row':rows')
        , ") AS "
        , qn
        ]
    ppSrc (Join jointype on left right) = do
      l' <- ppSql left
      r' <- ppSql right
      on' <- ppCol on
      lqn <- freshQueryName
      rqn <- freshQueryName
      pure $ mconcat
        [ " FROM (", l', ") AS ", lqn
        , " ",  ppJoinType jointype, " (", r', ") AS ", rqn
        , " ON ", on'
        ]

    ppJoinType LeftJoin  = "LEFT JOIN"
    ppJoinType InnerJoin = "JOIN"

    ppRow xs = do
      ls <- sequence [ppLit l | Param l <- xs]
      pure $ Text.intercalate ", " ls

    ppRestricts [] = pure ""
    ppRestricts rs = ppCols rs >>= \rs' -> pure $ " WHERE " <> rs'

    ppGroups [] = pure ""
    ppGroups grps = do
      cls <- sequence [ppCol c | Some c <- grps]
      pure $ " GROUP BY " <> Text.intercalate ", " cls

    ppOrder [] = pure ""
    ppOrder os = do
      os' <- sequence [(<> (" " <> ppOrd o)) <$> ppCol c | (o, Some c) <- os]
      pure $ " ORDER BY " <> Text.intercalate ", " os'

    ppOrd Asc = "ASC"
    ppOrd Desc = "DESC"

    ppLimit Nothing =
      pure ""
    ppLimit (Just (off, limit)) =
      pure $ " LIMIT " <> ppInt limit <> " OFFSET " <> ppInt off

    ppInt = Text.pack . show

ppSomeCol :: SomeCol SQL -> PP Text
ppSomeCol (Some c)    = ppCol c
ppSomeCol (Named n c) = do
  c' <- ppCol c
  pure $ c' <> " AS " <> fromColName n

ppCols :: [Exp SQL Bool] -> PP Text
ppCols cs = do
  cs' <- mapM ppCol (reverse cs)
  pure $ "(" <> Text.intercalate ") AND (" cs' <> ")"

ppType :: SqlTypeRep -> PP Text
ppType t = do
  c <- ppConfig <$> get
  pure $ Cfg.ppType c t

ppCol :: Exp SQL a -> PP Text
ppCol (TblCol xs)    = error $ "compiler bug: ppCol saw TblCol: " ++ show xs
ppCol (Col name)     = pure (fromColName name)
ppCol (Lit l)        = ppLit l
ppCol (BinOp op a b) = ppBinOp op a b
ppCol (UnOp op a)    = ppUnOp op a
ppCol (Fun2 f a b)   = do
  a' <- ppCol a
  b' <- ppCol b
  pure $ mconcat [f, "(", a', ", ", b', ")"]
ppCol (If a b c)     = do
  a' <- ppCol a
  b' <- ppCol b
  c' <- ppCol c
  pure $ mconcat ["CASE WHEN ", a', " THEN ", b', " ELSE ", c', " END"]
ppCol (AggrEx f x)   = ppUnOp (Fun f) x
ppCol (Cast t x)     = do
  x' <- ppCol x
  t' <- ppType t
  pure $ mconcat ["CAST(", x', " AS ", t', ")"]
ppCol (InList x xs) = do
  x' <- ppCol x
  xs' <- mapM ppCol xs
  pure $ mconcat [x', " IN (", Text.intercalate ", " xs', ")"]
ppCol (InQuery x q) = do
  x' <- ppCol x
  q' <- ppSql q
  pure $ mconcat [x', " IN (", q', ")"]

ppUnOp :: UnOp a b -> Exp SQL a -> PP Text
ppUnOp op c = do
  c' <- ppCol c
  pure $ case op of
    Abs    -> "ABS(" <> c' <> ")"
    Sgn    -> "SIGN(" <> c' <> ")"
    Neg    -> "-(" <> c' <> ")"
    Not    -> "NOT(" <> c' <> ")"
    IsNull -> "(" <> c' <> ") IS NULL"
    Fun f  -> f <> "(" <> c' <> ")"

ppBinOp :: BinOp a b -> Exp SQL a -> Exp SQL a -> PP Text
ppBinOp op a b = do
    a' <- ppCol a
    b' <- ppCol b
    pure $ paren a a' <> " " <> ppOp op <> " " <> paren b b'
  where
    paren :: Exp SQL a -> Text -> Text
    paren (Col{}) c = c
    paren (Lit{}) c = c
    paren _ c       = "(" <> c <> ")"

    ppOp :: BinOp a b -> Text
    ppOp Gt    = ">"
    ppOp Lt    = "<"
    ppOp Gte   = ">="
    ppOp Lte   = "<="
    ppOp Eq    = "="
    ppOp Neq   = "!="
    ppOp And   = "AND"
    ppOp Or    = "OR"
    ppOp Add   = "+"
    ppOp Sub   = "-"
    ppOp Mul   = "*"
    ppOp Div   = "/"
    ppOp Like  = "LIKE"
