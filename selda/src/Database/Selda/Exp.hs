{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, CPP, DataKinds #-}
-- | The expression type underlying 'Col'.
module Database.Selda.Exp where
import Database.Selda.SqlType
import Database.Selda.Types
import Data.Text (Text)

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol sql where
  Some  :: !(Exp sql a) -> SomeCol sql
  Named :: !ColName -> !(Exp sql a) -> SomeCol sql

data UntypedCol sql where
  Untyped :: !(Exp sql a) -> UntypedCol sql

-- | Turn a renamed column back into a regular one.
--   If the column was renamed, it will be represented by a literal column,
--   and not its original expression.
hideRenaming :: SomeCol sql -> UntypedCol sql
hideRenaming (Named n _) = Untyped (Col n)
hideRenaming (Some c)    = Untyped c

-- | Underlying column expression type, parameterised over the type of
--   SQL queries.
data Exp sql a where
  Col     :: !ColName -> Exp sql a
  Lit     :: !(Lit a) -> Exp sql a
  BinOp   :: !(BinOp a b) -> !(Exp sql a) -> !(Exp sql a) -> Exp sql b
  UnOp    :: !(UnOp a b) -> !(Exp sql a) -> Exp sql b
  NulOp   :: !(NulOp a) -> Exp sql a
  Fun2    :: !Text -> !(Exp sql a) -> !(Exp sql b) -> Exp sql c
  If      :: !(Exp sql Bool) -> !(Exp sql a) -> !(Exp sql a) -> Exp sql a
  Cast    :: !SqlTypeRep -> !(Exp sql a) -> Exp sql b
  AggrEx  :: !Text -> !(Exp sql a) -> Exp sql b
  InList  :: !(Exp sql a) -> ![Exp sql a] -> Exp sql Bool
  InQuery :: !(Exp sql a) -> !sql -> Exp sql Bool

data NulOp a where
  Fun0   :: Text -> NulOp a

data UnOp a b where
  Abs    :: UnOp a a
  Not    :: UnOp Bool Bool
  Neg    :: UnOp a a
  Sgn    :: UnOp a a
  IsNull :: UnOp (Maybe a) Bool
  Fun    :: Text -> UnOp a b

data BinOp a b where
  Gt    :: BinOp a Bool
  Lt    :: BinOp a Bool
  Gte   :: BinOp a Bool
  Lte   :: BinOp a Bool
  Eq    :: BinOp a Bool
  Neq   :: BinOp a Bool
  And   :: BinOp Bool Bool
  Or    :: BinOp Bool Bool
  Add   :: BinOp a a
  Sub   :: BinOp a a
  Mul   :: BinOp a a
  Div   :: BinOp a a
  Like  :: BinOp Text Bool

-- | Any type which may contain column names.
class Names a where
  -- | Get all column names used in the given expression.
  allNamesIn :: a -> [ColName]

instance Names a => Names [a] where
  allNamesIn = concatMap allNamesIn

instance Names sql => Names (Exp sql a) where
  allNamesIn (Col n)       = [n]
  allNamesIn (Lit _)       = []
  allNamesIn (BinOp _ a b) = allNamesIn a ++ allNamesIn b
  allNamesIn (UnOp _ a)    = allNamesIn a
  allNamesIn (NulOp _)     = []
  allNamesIn (Fun2 _ a b)  = allNamesIn a ++ allNamesIn b
  allNamesIn (If a b c)    = allNamesIn a ++ allNamesIn b ++ allNamesIn c
  allNamesIn (Cast _ x)    = allNamesIn x
  allNamesIn (AggrEx _ x)  = allNamesIn x
  allNamesIn (InList x xs) = concatMap allNamesIn (x:xs)
  allNamesIn (InQuery x q) = allNamesIn x ++ allNamesIn q

instance Names sql => Names (SomeCol sql) where
  allNamesIn (Some c)    = allNamesIn c
  allNamesIn (Named n c) = n : allNamesIn c

instance Names sql => Names (UntypedCol sql) where
  allNamesIn (Untyped c) = allNamesIn c
