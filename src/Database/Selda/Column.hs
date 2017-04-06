{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances #-}
-- | Columns and associated utility functions.
module Database.Selda.Column where
import Database.Selda.Table
import Data.String
import Data.Text (Text)

type family Cols s a where
  Cols s (a :*: b) = Col s a :*: Cols s b
  Cols s a         = Col s a

class Columns a where
  toTup :: [ColName] -> a

instance Columns b => Columns (Col s a :*: b) where
  toTup (x:xs) = C (Col x) :*: toTup xs
  toTup _      = error "too few elements to toTup"

instance Columns (Col s a) where
  toTup [x] = C (Col x)
  toTup []  = error "too few elements to toTup"
  toTup _   = error "too many elements to toTup"

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol where
  Some  :: Exp a -> SomeCol
  Named :: ColName -> Exp a -> SomeCol

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
newtype Col s a = C {unC :: Exp a}

-- | A unary operation. Note that the provided function name is spliced
--   directly into the resulting SQL query. Thus, this function should ONLY
--   be used to implement well-defined functions that are missing from Selda's
--   standard library, and NOT in an ad hoc manner during queries.
fun :: Text -> Col s a -> Col s b
fun f = liftC $ UnOp (Fun f)

-- | Like 'fun', but with two arguments.
fun2 :: Text -> Col s a -> Col s b -> Col s c
fun2 f = liftC2 (Fun2 f)

-- | Underlying column expression type, not tied to any particular query.
data Exp a where
  Col    :: ColName -> Exp a
  Lit    :: Lit a -> Exp a
  Null   :: Exp (Maybe a)
  BinOp  :: BinOp a b -> Exp a -> Exp a -> Exp b
  UnOp   :: UnOp a b -> Exp a -> Exp b
  Fun2   :: Text -> Exp a -> Exp b -> Exp c
  Cast   :: Exp a -> Exp b
  AggrEx :: Text -> Exp a -> Exp b

-- | Get all column names in the given expression.
allNamesIn :: Exp a -> [ColName]
allNamesIn (Col n)       = [n]
allNamesIn (Null)        = []
allNamesIn (Lit _)       = []
allNamesIn (BinOp _ a b) = allNamesIn a ++ allNamesIn b
allNamesIn (UnOp _ a)    = allNamesIn a
allNamesIn (Fun2 _ a b)  = allNamesIn a ++ allNamesIn b
allNamesIn (Cast x)      = allNamesIn x
allNamesIn (AggrEx _ x)  = allNamesIn x

data UnOp a b where
  Abs :: UnOp a a
  Not :: UnOp Bool Bool
  Neg :: UnOp a a
  Sgn :: UnOp a a
  Fun :: Text -> UnOp a b

data BinOp a b where
  Gt    :: BinOp a Bool
  Lt    :: BinOp a Bool
  Gte   :: BinOp a Bool
  Lte   :: BinOp a Bool
  Eq    :: BinOp a Bool
  Neq   :: BinOp a Bool
  Is    :: BinOp a Bool
  IsNot :: BinOp a Bool
  And   :: BinOp Bool Bool
  Or    :: BinOp Bool Bool
  Add   :: BinOp a a
  Sub   :: BinOp a a
  Mul   :: BinOp a a
  Div   :: BinOp a a
  Like  :: BinOp Text Bool

data Lit a where
  LitS :: Text   -> Lit Text
  LitI :: Int    -> Lit Int
  LitD :: Double -> Lit Double
  LitB :: Bool   -> Lit Bool

instance Show (Lit a) where
  show (LitS s) = show s
  show (LitI i) = show i
  show (LitD d) = show d
  show (LitB b) = show b

class SqlType a where
  literal :: a -> Col s a

instance SqlType Int where
  literal = C . Lit . LitI
instance SqlType Double where
  literal = C . Lit . LitD
instance SqlType Text where
  literal = C . Lit . LitS
instance SqlType Bool where
  literal = C . Lit . LitB

instance IsString (Col s Text) where
  fromString = literal . fromString

liftC2 :: (Exp a -> Exp b -> Exp c) -> Col s a -> Col s b -> Col s c
liftC2 f (C a) (C b) = C (f a b)

liftC :: (Exp a -> Exp b) -> Col s a -> Col s b
liftC f = C . f . unC

instance (SqlType a, Num a) => Num (Col s a) where
  fromInteger = literal . fromInteger
  (+) = liftC2 $ BinOp Add
  (-) = liftC2 $ BinOp Sub
  (*) = liftC2 $ BinOp Mul
  negate = liftC $ UnOp Neg
  abs = liftC $ UnOp Abs
  signum = liftC $ UnOp Sgn

instance Fractional (Col s Double) where
  fromRational = literal . fromRational
  (/) = liftC2 $ BinOp Div  

instance Fractional (Col s Int) where
  fromRational = literal . (truncate :: Double -> Int) . fromRational
  (/) = liftC2 $ BinOp Div
