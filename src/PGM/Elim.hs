{-# OPTIONS_GHC -Wall #-}

module PGM.Elim
       ( Context(..),
         mkContextFromFacs,
         mkContextFromRVEs,
         sumProdVE
       ) where

import PGM.Factor
import PGM.Vars
import Data.List


data Context = Context [RandVar] [Factor]
  deriving Show

mkContextFromFacs :: [Factor] -> Context
mkContextFromFacs facs = Context (collectVars facs []) facs
  where
    collectVars :: [Factor] -> [RandVar] -> [RandVar]
    collectVars []                 vars = vars
    collectVars (F newVars _:fs) vars =
      collectVars fs $
                  [newVar | newVar <- newVars, newVar `notElem` vars] ++ vars

mkContextFromRVEs :: [RandVarExpr] -> Context
mkContextFromRVEs = mkContextFromFacs . collectFactors

sumProdVE :: [RandVar] -> Context -> Factor
sumProdVE []     (Context _   facs) = fProduct facs
sumProdVE (v:vs) (Context cvs facs) = sumProdVE vs $
                                                Context (delete v cvs)
                                                        (sumProdElim facs v)

sumProdElim :: [Factor] -> RandVar -> [Factor]
sumProdElim facs var = msg:leftOverFacs
 where
  (elimFacs, leftOverFacs) = partition (scopeContains var) facs
  prd = fProduct elimFacs
  msg = marginalize var prd
