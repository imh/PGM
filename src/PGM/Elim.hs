{-# OPTIONS_GHC -Wall #-}

module PGM.Elim
       ( Context(..),
         mkContextFromFacs,
         mkContextFromRVEs,
         sumProdVE,
         maxCardElim,
         minFillElim
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
    maxCardRec [] varsEliminated = varsEliminated
    maxCardRec varsLeft varsEliminated =
      maxCardRec (delete elimVar varsLeft) $ elimVar:varsEliminated
      where
        addNeighbors var (numNeighbors, neighbors) (F fVars _) =
          if var `elem` fVars
            then (numNeighbors + length newNeighbors, neighbors ++ newNeighbors)
            else (numNeighbors, neighbors)
          where
            newNeighbors = [nbr | nbr <- fVars, nbr /= var, nbr `notElem` neighbors]
        cardRem v = fst $ foldl' (addNeighbors v) (0, []) facs
        elimVar = maximumBy (compare `on` cardRem) varsLeft

-- | Greedy search for constructing an elimination ordering
greedyElim :: Ord a
           => (Context -> RandVar -> a) -- ^ The function to select on
           -> Context                   -- ^ The context to search in
           -> [RandVar]                 -- ^ Variables to keep
           -> [RandVar]                 -- ^ Returned variable elimination ordering
greedyElim f c@(Context cVars _) varsToKeep = greedyElimRec c (cVars \\ varsToKeep) []
  where
    greedyElimRec :: Context -> [RandVar] -> [RandVar] -> [RandVar]
    greedyElimRec _ [] varsEliminated = varsEliminated
    greedyElimRec c_@(Context cVars_ facs_) varsLeft varsEliminated =
      greedyElimRec (Context (delete elimVar cVars_) $
                             newFac:reducedFacs)
                    (delete elimVar varsLeft)
                    (elimVar:varsEliminated)
      where
        reducedFacs = filter facHasVars $ map (dropVarFromFac elimVar) facs_
        elimVar     = minimumBy (compare `on` (f c_)) varsLeft
        newFac      = F (neighbors facs_ elimVar) $ error "Factors created in elimination ordering have no function."
        neighbors :: [Factor] -> RandVar -> [RandVar]
        neighbors facs v = foldl' (\vs (F fvs _) -> vs `union` fvs) [] $ filter (\(F fvs_ _) -> v `elem` fvs_) facs
        facHasVars :: Factor -> Bool
        facHasVars (F [] _) = False
        facHasVars _        = True
        dropVarFromFac :: RandVar -> Factor -> Factor
        dropVarFromFac var (F vars fun) = F (delete var vars) fun

edgesToAdd :: Context -> RandVar -> Int
edgesToAdd (Context _ facs) var = foldl' undefined undefined undefined

minFillElim :: Context    -- ^ The context to eliminate from
            -> [RandVar]  -- ^ Variables to keep
            -> [RandVar]  -- ^ Variable elimination ordering
minFillElim = greedyElim edgesToAdd