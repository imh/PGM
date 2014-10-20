{-# OPTIONS_GHC -Wall #-}

module PGM.Vars where

import Data.List
import Numeric
import Data.Tuple (swap)
--import qualified Data.Set as Set

type Val = Rational

--TODO generalize for notjust rationals, but anything instantiating Eq (and Ord?)
data RandVarExpr = TopLevel String [(Val, Double)]
                 | Const Val
                 | Add RandVarExpr RandVarExpr
                 | Mul RandVarExpr RandVarExpr
                 | Abs RandVarExpr
                 | Signum RandVarExpr

instance Num RandVarExpr where
  (+) = Add
  (*) = Mul
  negate = Mul (Const (-1))
  abs = Abs
  signum = Signum
  fromInteger = Const . fromInteger

instance Eq RandVarExpr where
  (TopLevel name1 _) == (TopLevel name2 _) = name1 == name2
  (Const x)          == (Const y)          = x     == y
  (Add x y)          == (Add w v)          = (x == w && y == v) || (x == v && y == w)
  (Mul x y)          == (Mul w v)          = (x == w && y == v) || (x == v && y == w)
  (Abs x)            == (Abs y)            = x     == y
  (Signum x)         == (Signum y)         = x     == y
  _                  == _                  = False

instance Show RandVarExpr where
  show (TopLevel name _) = name
  show (Const x) = show (fromRat x :: Double)
  show (Add x y) = "(" ++ show x ++ ") + (" ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ ") * (" ++ show y ++ ")"
  show (Abs x) = "|" ++ show x ++ "|"
  show (Signum x) = "signum(" ++ show x ++ ")"

type Ident = RandVarExpr

--                       Id     Values
data RandVar = RV { rvIdent :: Ident
                  , rvVals :: [Val]
                  }
                  deriving (Eq)
instance Show RandVar where
  show (RV i _) = show i
instance Ord RandVar where
  compare (RV i1 _) (RV i2 _) = if i1 == i2 then EQ else compare (show i1) (show i2)

-- Note that this by default makes all potentially concievable values from the structure of
-- the expressions, not from their semantics.
-- (vals $ Mul x x) can suggest Mul x x can take negative values, for example.
-- Only from other more global computations can we see that these will always
-- have probability 0.
-- If I specialize the function, I will have to do it over all cases
-- For now, it's just specialized for a few special cases
valsRVE :: RandVarExpr -> [Val]
valsRVE (Mul x (Const y)) = nub [valX * y | valX <- valsRVE x] -- special cases first
valsRVE (Mul (Const x) y) = nub [x * valY | valY <- valsRVE y]
--valsRVE (Add x x)      = valsRVE (Mul x $ Const 2) -- Apparently this kind of syntax in invalid :(
--valsRVE (Add x (Mul (Const y) x)) = valsRVE $ Mul $ Const (y + 1) $ x
--valsRVE (Add x (Mul x (Const y))) = valsRVE $ Mul $ Const (y + 1) $ x
--valsRVE (Add (Mul (Const y) x) x) = valsRVE $ Mul $ Const (y + 1) $ x
--valsRVE (Add (Mul x (Const y)) x) = valsRVE $ Mul $ Const (y + 1) $ x
--valsRVE (Mul x x) = nub $ [valX * valX | valX <- valsRVE]
valsRVE (TopLevel _ l) = nub $ map fst l -- Then defaults
valsRVE (Const x)      = [x]
valsRVE (Add x y)      = nub [valX + valY | valX <- valsRVE x, valY <- valsRVE y]
valsRVE (Mul x y)      = nub [valX * valY | valX <- valsRVE x, valY <- valsRVE y]
valsRVE (Abs x)        = nub [abs valX | valX <- valsRVE x]
valsRVE (Signum x)     = nub [signum valX | valX <- valsRVE x]
-- which way is more idiomatic of the following?
--  valsRVE (Abs x) = nub [abs valX | valX <- (valsRVE x)]
--  valsRVE (Abs x) = nub $ map abs $ valsRVE x

collectRVEs :: [RandVarExpr] -> [RandVarExpr]
collectRVEs rves = collectMoreRVEs rves []
  where collectMoreRVEs :: [RandVarExpr] -> [RandVarExpr] -> [RandVarExpr]
        collectMoreRVEs [] lst = lst
        collectMoreRVEs (v:vs) lst =
          if v `elem` lst
            then collectMoreRVEs vs lst
            else collectMoreRVEs (parents v ++ vs) (v:lst)
        parents :: RandVarExpr -> [RandVarExpr]
        parents (TopLevel _ _) = []
        parents (Const _)      = []
        parents (Add x y)      = [x, y]
        parents (Mul x y)      = [x, y]
        parents (Abs x)        = [x]
        parents (Signum x)     = [x]

mkMapToInts :: [RandVarExpr] -> [(RandVarExpr, Int)]
mkMapToInts = scanl (\vi v-> (v, 1 + snd vi)) (Const 0, 0)

mkMapFromInts :: [RandVarExpr] -> [(Int, RandVarExpr)]
mkMapFromInts = scanl (\vi v-> (1 + fst vi, v)) (0, Const 0)

swapMap :: [(a, b)] -> [(b, a)]
swapMap = map swap

makeRV :: RandVarExpr -> RandVar
makeRV v = RV v $ valsRVE v

lookupProb :: Val -> [(Val, Double)] -> Maybe Double
lookupProb _ [] = Nothing
lookupProb v (x:xs) =
  if v == fst x
    then Just $ snd x
    else lookup v xs
