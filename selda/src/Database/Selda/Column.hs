{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances #-}
-- | Columns and associated utility functions.
module Database.Selda.Column
  ( Cols, Columns
  , Col (..), SomeCol (..), Exp (..), UnOp (..), BinOp (..)
  , toTup, fromTup, liftC, liftC2
  , allNamesIn
  , literal
  ) where
import Database.Selda.SqlType
import Database.Selda.Types
import Data.String
import Data.Text (Text)

-- | Convert a tuple of Haskell types to a tuple of column types.
type family Cols s a where
  Cols s (a :*: b)      = Col s a :*: Cols s b
  Cols s a              = Col s a

-- | Any column tuple.
class Columns a where
  toTup :: [ColName] -> a
  fromTup :: a -> [SomeCol]

instance Columns b => Columns (Col s a :*: b) where
  toTup (x:xs) = C (Col x) :*: toTup xs
  toTup _      = error "too few elements to toTup"
  fromTup (C x :*: xs) = Some x : fromTup xs

instance Columns (Col s a) where
  toTup [x] = C (Col x)
  toTup []  = error "too few elements to toTup"
  toTup xs  = C (TblCol xs)
  fromTup (C x) = [Some x]

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol where
  Some  :: !(Exp a) -> SomeCol
  Named :: !ColName -> !(Exp a) -> SomeCol

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
newtype Col s a = C {unC :: Exp a}

-- | A literal expression.
literal :: SqlType a => a -> Col s a
literal = C . Lit . mkLit

-- | Underlying column expression type, not tied to any particular query.
data Exp a where
  Col    :: !ColName -> Exp a
  TblCol :: ![ColName] -> Exp a
  Lit    :: !(Lit a) -> Exp a
  BinOp  :: !(BinOp a b) -> !(Exp a) -> !(Exp a) -> Exp b
  UnOp   :: !(UnOp a b) -> !(Exp a) -> Exp b
  Fun2   :: !Text -> !(Exp a) -> !(Exp b) -> Exp c
  Cast   :: !Text -> !(Exp a) -> Exp b
  AggrEx :: !Text -> !(Exp a) -> Exp b
  InList :: !(Exp a) -> ![Exp a] -> Exp Bool

-- | Get all column names in the given expression.
allNamesIn :: Exp a -> [ColName]
allNamesIn (TblCol ns)   = ns
allNamesIn (Col n)       = [n]
allNamesIn (Lit _)       = []
allNamesIn (BinOp _ a b) = allNamesIn a ++ allNamesIn b
allNamesIn (UnOp _ a)    = allNamesIn a
allNamesIn (Fun2 _ a b)  = allNamesIn a ++ allNamesIn b
allNamesIn (Cast _ x)    = allNamesIn x
allNamesIn (AggrEx _ x)  = allNamesIn x
allNamesIn (InList x xs) = concatMap allNamesIn (x:xs)

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

instance {-# OVERLAPPING #-} (SqlType a, Num a) => Num (Col s (Maybe a)) where
  fromInteger = literal . Just . fromInteger
  (+) = liftC2 $ BinOp Add
  (-) = liftC2 $ BinOp Sub
  (*) = liftC2 $ BinOp Mul
  negate = liftC $ UnOp Neg
  abs = liftC $ UnOp Abs
  signum = liftC $ UnOp Sgn

instance Fractional (Col s Double) where
  fromRational = literal . fromRational
  (/) = liftC2 $ BinOp Div  

instance Fractional (Col s (Maybe Double)) where
  fromRational = literal . Just . fromRational
  (/) = liftC2 $ BinOp Div  

instance Fractional (Col s Int) where
  fromRational = literal . (truncate :: Double -> Int) . fromRational
  (/) = liftC2 $ BinOp Div

instance Fractional (Col s (Maybe Int)) where
  fromRational = literal . Just . (truncate :: Double -> Int) . fromRational
  (/) = liftC2 $ BinOp Div
