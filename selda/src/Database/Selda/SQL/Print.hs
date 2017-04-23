{-# LANGUAGE GADTs, OverloadedStrings #-}
-- | Pretty-printing for SQL queries. For some values of pretty.
module Database.Selda.SQL.Print where
import Database.Selda.Column
import Database.Selda.SQL
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
  { ppParams  :: [Param]
  , ppTables  :: [TableName]
  , ppParamNS :: Int
  , ppQueryNS :: Int
  }

-- | Run a pretty-printer.
runPP :: PP Text -> ([TableName], (Text, [Param]))
runPP pp =
  case runState pp (PPState [] [] 1 0) of
    (q, st) -> (snub $ ppTables st, (q, reverse (ppParams st)))

-- | Compile an SQL AST into a parameterized SQL query.
compSql :: SQL -> ([TableName], (Text, [Param]))
compSql = runPP . ppSql

-- | Compile a single column expression.
compExp :: Exp a -> (Text, [Param])
compExp = snd . runPP . ppCol

-- | Compile an @UPATE@ statement.
compUpdate :: TableName -> Exp Bool -> [(ColName, SomeCol)] -> (Text, [Param])
compUpdate tbl p cs = snd $ runPP ppUpd
  where
    ppUpd = do
      updates <- mapM ppUpdate cs
      check <- ppCol p
      pure $ Text.unwords
        [ "UPDATE", tbl
        , "SET", set updates
        , "WHERE", check
        ]
    ppUpdate (n, c) = do
      c' <- ppSomeCol c
      let upd = Text.unwords [n, "=", c']
      if n == c'
        then pure $ Left upd
        else pure $ Right upd
    -- if the update doesn't change anything, pick an arbitrary column to
    -- set to itself just to satisfy SQL's syntactic rules
    set us =
      case [u | Right u <- us] of
        []  -> set (take 1 [Right u | Left u <- us])
        us' -> Text.intercalate ", " us'

-- | Compile a @DELETE@ statement.
compDelete :: TableName -> Exp Bool -> (Text, [Param])
compDelete tbl p = snd $ runPP ppDelete
  where
    ppDelete = do
      c' <- ppCol p
      pure $ Text.unwords ["DELETE FROM", tbl, "WHERE", c']

-- | Pretty-print a literal as a named parameter and save the
--   name-value binding in the environment.
ppLit :: Lit a -> PP Text
ppLit LitNull     = pure "NULL"
ppLit (LitJust l) = ppLit l
ppLit l           = do
  PPState ps ts ns qns <- get
  put $ PPState (Param l : ps) ts (succ ns) qns
  return $ Text.pack ('$':show ns)

dependOn :: TableName -> PP ()
dependOn t = do
  PPState ps ts ns qns <- get
  put $ PPState ps (t:ts) ns qns

-- | Generate a unique name for a subquery.
freshQueryName :: PP Text
freshQueryName = do
  PPState ps ts ns qns <- get
  put $ PPState ps ts ns (succ qns)
  return $ Text.pack ('q':show qns)

-- | Pretty-print an SQL AST.
ppSql :: SQL -> PP Text
ppSql (SQL cs src r gs ord lim) = do
  cs' <- mapM ppSomeCol cs
  src' <- ppSrc src
  r' <- ppRestricts r
  gs' <- ppGroups gs
  ord' <- ppOrder ord
  lim' <- ppLimit lim
  pure $ mconcat
    [ "SELECT ", result cs'
    , src'
    , r'
    , gs'
    , ord'
    , lim'
    ]
  where
    result []  = "1"
    result cs' = Text.intercalate "," cs'

    ppSrc EmptyTable = do
      qn <- freshQueryName
      pure $ " FROM (SELECT NULL LIMIT 0) AS " <> qn
    ppSrc (TableName n)  = do
      dependOn n
      pure $ " FROM " <> n
    ppSrc (Product [])   = do
      pure ""
    ppSrc (Product sqls) = do
      srcs <- mapM ppSql (reverse sqls)
      qs <- flip mapM ["(" <> s <> ")" | s <- srcs] $ \q -> do
        qn <- freshQueryName
        pure (q <> " AS " <> qn)
      pure $ " FROM " <> Text.intercalate "," qs
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
    ppSrc (LeftJoin on left right) = do
      l' <- ppSql left
      r' <- ppSql right
      on' <- ppCol on
      lqn <- freshQueryName
      rqn <- freshQueryName
      pure $ mconcat
        [ " FROM (", l', ") AS ", lqn
        , " LEFT JOIN (", r', ") AS ", rqn
        , " ON ", on'
        ]

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

ppSomeCol :: SomeCol -> PP Text
ppSomeCol (Some c)    = ppCol c
ppSomeCol (Named n c) = do
  c' <- ppCol c
  pure $ c' <> " AS " <> n

ppCols :: [Exp Bool] -> PP Text
ppCols cs = do
  cs' <- mapM ppCol (reverse cs)
  pure $ "(" <> Text.intercalate ") AND (" cs' <> ")"

ppCol :: Exp a -> PP Text
ppCol (TblCol xs)    = error $ "compiler bug: ppCol saw TblCol: " ++ show xs
ppCol (Col name)     = pure name
ppCol (Lit l)        = ppLit l
ppCol (BinOp op a b) = ppBinOp op a b
ppCol (UnOp op a)    = ppUnOp op a
ppCol (Fun2 f a b)   = do
  a' <- ppCol a
  b' <- ppCol b
  pure $ mconcat [f, "(", a', ", ", b', ")"]
ppCol (AggrEx f x)   = ppUnOp (Fun f) x
ppCol (Cast x)       = ppCol x

ppUnOp :: UnOp a b -> Exp a -> PP Text
ppUnOp op c = do
  c' <- ppCol c
  pure $ case op of
    Abs    -> "ABS(" <> c' <> ")"
    Sgn    -> "SIGN(" <> c' <> ")"
    Neg    -> "-(" <> c' <> ")"
    Not    -> "NOT(" <> c' <> ")"
    IsNull -> "(" <> c' <> ") IS NULL"
    Fun f  -> f <> "(" <> c' <> ")"

ppBinOp :: BinOp a b -> Exp a -> Exp a -> PP Text
ppBinOp op a b = do
    a' <- ppCol a
    b' <- ppCol b
    pure $ paren a a' <> " " <> ppOp op <> " " <> paren b b'
  where
    paren :: Exp a -> Text -> Text
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
