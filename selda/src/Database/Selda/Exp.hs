{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, CPP, DataKinds #-}
-- | The expression type underlying 'Col'.
module Database.Selda.Exp where
import Database.Selda.SqlType
import Database.Selda.Types hiding ((:*:))
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import qualified GHC.TypeLits as TL

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
  TblCol  :: ![ColName] -> Exp sql a
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
  allNamesIn (TblCol ns)   = ns
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

class GExp f where
  gExpressions :: f x -> [UntypedCol sql]

instance GExp f => GExp (D1 x f) where
  gExpressions (M1 x) = gExpressions x

instance GExp f => GExp (C1 x f) where
  gExpressions (M1 x) = gExpressions x

instance GExp f => GExp (S1 ('MetaSel x y z 'DecidedLazy) f) where
  gExpressions (M1 x) = gExpressions x

#if MIN_VERSION_base(4, 9, 0)
instance
  (TL.TypeError
    ('TL.Text "Types with strict fields are not usable within queries.")
  ) => GExp (S1 ('MetaSel x y z 'DecidedStrict) f) where
  gExpressions _ = error "unreachable"
#endif

instance (Typeable a, SqlType a) => GExp (K1 i a) where
  gExpressions (K1 x) = [Untyped (Lit (mkLit x))]

instance (GExp a, GExp b) => GExp (a :*: b) where
  gExpressions (a :*: b) = gExpressions a ++ gExpressions b
