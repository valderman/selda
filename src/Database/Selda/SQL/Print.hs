{-# LANGUAGE GADTs, OverloadedStrings #-}
-- | Pretty-printing for SQL queries. For some values of pretty.
module Database.Selda.SQL.Print where
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import Database.Selda.SQL
import Database.Selda.Column

-- | SQL pretty-printer. The state is the list of SQL parameters to the
--   prepared statement.
type PP = State [Param]

-- | Compile an SQL AST into a parameterized SQL query.
compSql :: SQL -> (Text, [Param])
compSql sql =
  case runState (ppSql sql) [] of
    (q, ps) -> (q <> ";", reverse ps)

-- | Pretty-print a literal as a named parameter and save the
--   name-value binding in the environment.
ppLit :: Lit a -> PP Text
ppLit l = do
  ps <- get
  put (Param l : ps)
  return "?"

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
    [ "SELECT ", Text.intercalate "," cs'
    , src'
    , r'
    , gs'
    , ord'
    , lim'
    ]
  where
    ppSrc (Left n)     = pure $ " FROM " <> n
    ppSrc (Right [])   = pure ""
    ppSrc (Right sqls) = do
      srcs <- mapM ppSql (reverse sqls)
      pure $ " FROM " <> Text.intercalate "," ["(" <> s <> ")" | s <- srcs]

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

    ppLimit Nothing = pure ""
    ppLimit (Just (from, to)) = pure $ " LIMIT " <> ppInt from <> "," <> ppInt to

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
    Abs   -> "ABS(" <> c' <> ")"
    Sgn   -> "SIGN(" <> c' <> ")"
    Neg   -> "-(" <> c' <> ")"
    Not   -> "NOT(" <> c' <> ")"
    Fun f -> f <> "(" <> c' <> ")"

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
    ppOp Gt   = ">"
    ppOp Lt   = "<"
    ppOp Gte  = ">="
    ppOp Lte  = "<="
    ppOp Eq   = "="
    ppOp And  = "AND"
    ppOp Or   = "OR"
    ppOp Add  = "+"
    ppOp Sub  = "-"
    ppOp Mul  = "*"
    ppOp Div  = "/"
    ppOp Like = "LIKE"
