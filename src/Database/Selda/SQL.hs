{-# LANGUAGE GADTs, OverloadedStrings #-}
-- | SQL AST and pretty-printing.
module Database.Selda.SQL where
import Database.Selda.Table
import Database.Selda.Column
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid

-- | AST for SQL queries.
data SQL = SQL
  { cols      :: [SomeCol]
  , source    :: Either TableName [SQL]
  , restricts :: [Exp Bool]
  , groups    :: [SomeCol]
  , ordering  :: [(Order, SomeCol)]
  , limits    :: Maybe (Int, Int)
  }

-- | The order in which to sort result rows.
data Order = Asc | Desc
  deriving (Show, Ord, Eq)

data Param where
  Param :: Lit a -> Param

instance Show Param where
  show (Param l) = "Param " <> show l

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
    ppGroups gs = do
      cs <- sequence [ppCol c | Some c <- gs]
      pure $ " GROUP BY " <> Text.intercalate ", " cs

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
  c <- ppCol c
  pure $ c <> " AS " <> n

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
