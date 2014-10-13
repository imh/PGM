{-# OPTIONS_GHC -Wall #-}

module PGM.Graph where


import PGM.Factor
import PGM.Vars
import Data.List

data FactorGraph = FG {fgVars :: [RandomVariable], fgFacs :: [IdentifiedFactor]} deriving (Show)

makePrimalGraph :: [RandomVariableExpr] -> FactorGraph
makePrimalGraph xs = addToPrimalGraph xs (FG [] [])

addToPrimalGraph :: [RandomVariableExpr] -> FactorGraph -> FactorGraph
addToPrimalGraph [] x = x
addToPrimalGraph (v@(TopLevel _ _):vs) g@(FG rvs fs) =
  if (elem (makeIFactor v) fs)
    then (addToPrimalGraph vs g)
    else (addToPrimalGraph vs $
          FG ((makeRV v):rvs) ((makeIFactor v):fs))
addToPrimalGraph (v@(Const _):vs) g@(FG rvs fs) =
  if (elem (makeIFactor v) fs)
    then (addToPrimalGraph vs g)
    else (addToPrimalGraph vs $
          FG ((makeRV v):rvs) ((makeIFactor v):fs))
addToPrimalGraph (v@(Add x y):vs) g@(FG rvs fs) =
  if (elem (makeIFactor v) fs)
    then (addToPrimalGraph vs g)
    else (addToPrimalGraph (x:y:vs) $
          FG ((makeRV v):rvs) ((makeIFactor v):fs))
addToPrimalGraph (v@(Mul x y):vs) g@(FG rvs fs) =
  if (elem (makeIFactor v) fs)
    then (addToPrimalGraph vs g)
    else (addToPrimalGraph (x:y:vs) $
          FG ((makeRV v):rvs) ((makeIFactor v):fs))
addToPrimalGraph (v@(Abs x):vs) g@(FG rvs fs) =
  if (elem (makeIFactor v) fs)
    then (addToPrimalGraph vs g)
    else (addToPrimalGraph (x:vs) $
          FG ((makeRV v):rvs) ((makeIFactor v):fs))
addToPrimalGraph (v@(Signum x):vs) g@(FG rvs fs) =
  if (elem (makeIFactor v) fs)
    then (addToPrimalGraph vs g)
    else (addToPrimalGraph (x:vs) $
          FG ((makeRV v):rvs) ((makeIFactor v):fs))

sumProdVE :: [RandomVariable] -> [Factor] -> Factor
sumProdVE [] facs = fProds (facs)
sumProdVE (x:xs) facs = sumProdVE xs (sumProdElim facs x)

sumProdElim :: [Factor] -> RandomVariable -> [Factor]
sumProdElim facs var = (msg:leftOverFacs) where
  (elimFacs, leftOverFacs) = partition (scopeContains var) facs
  prd = fProds elimFacs
  msg = marginalize var prd
