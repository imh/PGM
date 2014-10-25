{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module PGM.Factor
       ( Factor(..),
         fProduct,
         marginalize,
         collectFactors,
         scopeContains,
         mkFactor
       ) where

import PGM.Vars
import qualified Data.Map.Strict as M
import Data.List


data Asgn a b = a := b
instance Show (Asgn RandVar Val) where
  show (a := b) = show a ++ " := " ++ show (fromRational b :: Double)
instance (Eq a, Eq b) => Eq (Asgn a b) where
  (aVar := aVal) == (bVar := bVal) = aVar == bVar && aVal == bVal

-- pseudo probabilty, depending on the vars, can be a probability or a conditional probability, or fully general factor (as in Koller & Friedman)
--               vars             val vector-> pseudo probability
-- TODO generalize Val, and more importantly get abstract functions
data FFun = Table [([Asgn RandVar Val], Double)]
  deriving Show
toTabular :: FFun -> [([Asgn RandVar Val], Double)]
toTabular (Table t) = t

data IdentifiedFactor = IF Ident Factor
instance Eq IdentifiedFactor where
  (IF i1 _) == (IF i2 _) = i1 == i2
instance Show IdentifiedFactor where
    show (IF vs _) = show vs

data Factor = F [RandVar] FFun
  deriving Show

-- | Checks whether a RandVar is contained in the scope of a Factor
scopeContains :: RandVar -- ^ The variable to be looked up
              -> Factor  -- ^ The factor that may contain the variable
              -> Bool    -- ^ Whether the factor contains the variable
scopeContains var (F vars _) = var `elem` vars

fProduct :: [Factor] -> Factor
fProduct []      = F [] $ Table []
fProduct [x]     = x
fProduct (x1:xs) = fProd x1 $ fProduct xs
                   where
                    fProd (F xVars xFun) (F yVars yFun) =
                      F (xVars `union` yVars)
                        (funCombine xFun yFun)

funCombine :: FFun -> FFun -> FFun
funCombine (Table xs) (Table ys) =
  Table [(xAsgn `union` yAsgn, xProb * yProb) | (xAsgn, xProb) <- xs,
                                                (yAsgn, yProb) <- ys,
                                                assignmentsCompatible xAsgn yAsgn]

assignmentsCompatible :: [Asgn RandVar Val] -> [Asgn RandVar Val] -> Bool
assignmentsCompatible xs ys =
  all valsMatch [(xVal, yVal) | xVar := xVal <- xs,
                                yVar := yVal <- ys,
                                xVar == yVar]

valsMatch :: (Val, Val) -> Bool
valsMatch (x, y) = x == y

-- TODO make this shit clearer
marginalize :: RandVar -> Factor -> Factor
marginalize arg (F vars fun) =
  F (filter (not . (== arg)) vars) $
    Table . fromPairs .M.toList $ M.foldrWithKey step M.empty $ M.fromList $ toPairs $ toTabular fun
  where
   fromPairs     = map (\(a,b) -> (map (\(x,y)    -> x := y) a,b))
   toPairs       = map (\(a,b) -> (map (\(x := y) -> (x,y) ) a,b))
   step as c acc =
     let as' = filter (not . (== arg) . fst) as
     in
      M.insertWith (+) as' c acc

collectFactors :: [RandVarExpr] -> [Factor]
collectFactors varExprs = map mkFactor $
                              collectRVEs varExprs

mkFactor :: RandVarExpr -> Factor
mkFactor v@(TopLevel _ list) = F [vVar] $
                                 Table (map (\x -> ([vVar := fst x],
                                                    snd x))
                                            list)
                               where
                                 vVar = mkRV v
mkFactor v@(Const x)         = F [vVar] $
                                 Table [([vVar := x],
                                         1)]
                               where
                                 vVar = mkRV v
mkFactor v@(Add x y)         = F [xVar, yVar, vVar] $
                                 Table [([xVar := xVal,
                                          yVar := yVal,
                                          vVar := (xVal + yVal)],
                                         1) | xVal <- rvVals xVar,
                                              yVal <- rvVals yVar]
                               where
                                 xVar = mkRV x
                                 yVar = mkRV y
                                 vVar = mkRV v
mkFactor v@(Mul x y)         = F [xVar, yVar, vVar] $
                                 Table [([xVar := xVal,
                                          yVar := yVal,
                                          vVar := (xVal * yVal)],
                                         1) | xVal <- rvVals xVar,
                                              yVal <- rvVals yVar]
                               where
                                 xVar = mkRV x
                                 yVar = mkRV y
                                 vVar = mkRV v
mkFactor v@(Abs x)           = F [xVar, vVar] $
                                 Table [([xVar := xVal,
                                          vVar := abs xVal],
                                         1) | xVal <- rvVals xVar]
                               where
                                 xVar = mkRV x
                                 vVar = mkRV v
mkFactor v@(Signum x)        = F [xVar, vVar] $
                                 Table [([xVar := xVal,
                                          vVar := signum xVal],
                                         1) | xVal <- rvVals xVar]
                               where
                                 xVar = mkRV x
                                 vVar = mkRV v