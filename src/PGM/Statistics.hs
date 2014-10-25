{-# OPTIONS_GHC -Wall #-}

module PGM.Statistics
       ( expectation
       ) where

import PGM.Factor
import PGM.Vars
import PGM.Elim

expectation :: Context -> RandVar -> Double
expectation cg v = facExpectation finalFac
  where
    ord = defaultElim cg [v]
    finalFac = sumProdVE ord cg

facExpectation :: Factor -> Double
facExpectation (F vs (Table funEntries)) =
  weightedSum / normalization
  where
    fstVal [] = error "facExpectation: Expectation requires factor with variables"
    fstVal (var := val:[]) = val
    fstVal _ = error "facExpectation: Expectation requires factor with single variable"
    weightedSum = foldl (\acc (asgns, prob) -> (fromRational $ fstVal asgns) * prob + acc) 0 funEntries
    normalization = sum $ map snd funEntries