{-# LANGUAGE GADTs #-}
-- | SQL AST and pretty-printing.
module Database.Selda.SQL where
import Database.Selda.Table
import Database.Selda.Column
import Control.Monad.State
import Data.List (intercalate)

-- | AST for SQL queries.
data SQL = SQL
  { cols      :: [SomeCol]
  , source    :: Either TableName [SQL]
  , restricts :: [Col Bool]
  }

data Param where
  Param :: Lit a -> Param

instance Show Param where
  show (Param l) = "Param " ++ show l

type PP = State [Param]

-- | Compile an SQL AST into a parameterized SQL query.
compSql :: SQL -> (String, [Param])
compSql sql =
  case runState (ppSql sql) [] of
    (q, ps) -> (q ++ ";", reverse ps)

-- | Pretty-print a literal as a named parameter and save the
--   name-value binding in the environment.
ppLit :: Lit a -> PP String
ppLit l = do
  ps <- get
  put (Param l : ps)
  return "?"

-- | Pretty-print an SQL AST.
ppSql :: SQL -> PP String
ppSql (SQL cs src r) = do
  cs' <- mapM ppSomeCol cs
  src' <- ppSrc src
  r' <- ppRestricts r
  pure $ concat
    [ "SELECT ", intercalate "," cs'
    , " FROM ", src'
    , r'
    ]
  where
    ppSrc (Left n)     = pure n
    ppSrc (Right sqls) = do
      srcs <- mapM ppSql (reverse sqls)
      pure $ intercalate "," ["(" ++ s ++ ")" | s <- srcs]

    ppRestricts [] = pure ""
    ppRestricts rs = ppCols rs >>= \rs' -> pure $ " WHERE " ++ rs'

ppSomeCol :: SomeCol -> PP String
ppSomeCol (Some c)    = ppCol c
ppSomeCol (Named n c) = do
  c <- ppCol c
  pure $ c ++ " AS " ++ n

ppCols :: [Col Bool] -> PP String
ppCols cs = do
  cs' <- mapM ppCol (reverse cs)
  pure $ "(" ++ intercalate ") AND (" cs' ++ ")"

ppCol :: Col a -> PP String
ppCol (Col name)     = pure name
ppCol (Lit l)        = ppLit l
ppCol (BinOp op a b) = ppBinOp op a b
ppCol (UnOp op a)    = ppUnOp op a

ppUnOp :: UnOp a b -> Col a -> PP String
ppUnOp op c = do
  c' <- ppCol c
  pure $ case op of
    Abs -> "ABS(" ++ c' ++ ")"
    Sgn -> "SIGN(" ++ c' ++ ")"
    Neg -> "-(" ++ c' ++ ")"
    Not -> "NOT(" ++ c' ++ ")"

ppBinOp :: BinOp a b -> Col a -> Col a -> PP String
ppBinOp op a b = do
    a' <- ppCol a
    b' <- ppCol b
    pure $ paren a a' ++ " " ++ ppOp op ++ " " ++ paren b b'
  where
    paren :: Col a -> String -> String
    paren (Col{}) c = c
    paren (Lit{}) c = c
    paren _ c       = "(" ++ c ++ ")"

    ppOp :: BinOp a b -> String
    ppOp Gt   = ">"
    ppOp Lt   = "<"
    ppOp Gte  = ">="
    ppOp Lte  = "<="
    ppOp Eq   = "="
    ppOp Add  = "+"
    ppOp Sub  = "-"
    ppOp Mul  = "*"
    ppOp Div  = "/"
    ppOp Like = "LIKE"
