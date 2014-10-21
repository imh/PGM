{-# OPTIONS_GHC -Wall #-}

module PGM.Elim
       ( Context(..),
         mkContextFromFacs,
         mkContextFromRVEs,
         sumProdVE,
         maxCardElim
       ) where

import PGM.Factor
import PGM.Vars
import Data.List
import Data.Function(on)


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

-- | Maximum cardinality search for constructing an elimination ordering
maxCardElim :: Context   -- ^ The model to choose elimination on
            -> [RandVar] -- ^ Variables to keep
            -> [RandVar] -- ^ Returned variable elimination ordering
maxCardElim (Context cVars facs) varsToKeep = maxCardRec (cVars \\ varsToKeep) []
  where
    maxCardRec :: [RandVar] -> [RandVar] -> [RandVar]
    maxCardRec varsLeft varsEliminated =
      maxCardRec (delete elimVar varsLeft) $ elimVar:varsEliminated
      where
        addNeighbors var (numNeighbors, neighbors) (F fVars _) =
          if var `elem` fVars
            then (numNeighbors + length newNeighbors, neighbors ++ newNeighbors)
            else (numNeighbors, neighbors)
          where
            newNeighbors = [nbr | nbr <- fVars, nbr /= var, nbr `notElem` neighbors]
        cardRem v = fst $ foldl (addNeighbors v) (0, []) facs
        elimVar = maximumBy (compare `on` cardRem) varsLeft

