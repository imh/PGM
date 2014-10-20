{-# OPTIONS_GHC -Wall #-}

module Main
       ( main
       ) where

import PGM.Factor
import PGM.Elim
import PGM.Vars

x, y, z, a, b, c :: RandVarExpr
x = TopLevel "x" [(-1, 0.5), (1, 0.5)]
y = 3
z = TopLevel "z" [(-1, 0.25), (0, 0.5), (1, 0.5)]
a = x + y
b = a * z
c = b * x

main :: IO ()
main = do
  putStrLn $ show c
  putStrLn $ show $ makeIFactor c
