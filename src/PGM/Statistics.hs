{-# OPTIONS_GHC -Wall #-}

module PGM.Statistics
       ( expectation,
         facMax
       ) where

import Data.List
import Data.Function(on)

import PGM.Factor
import PGM.Vars
import PGM.Elim


expectation :: Context -> RandVar -> Double
expectation cg v = facExpectation finalFac
  where
    ord = defaultElim cg [v]
    finalFac = sumProdVE ord cg

facExpectation :: Factor -> Double
facExpectation (F _ (Table funEntries)) =
  weightedSum / normalization
  where
    fstVal [] = error "facExpectation: Expectation requires factor with variables"
    fstVal (_ := val:[]) = val
    fstVal _ = error "facExpectation: Expectation requires factor with single variable"
    addWeightedFstVal acc (asgns, prob) = (fromRational $ fstVal asgns) * prob + acc
    weightedSum = foldl addWeightedFstVal 0 funEntries
    normalization = sum $ map snd funEntries

facMax :: Factor -> [Asgn RandVar Val]
facMax (F _ (Table funEntries)) =
  fst $ maximumBy (compare `on` snd) funEntries