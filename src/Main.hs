{-# OPTIONS_GHC -Wall #-}

module Main
       ( main
       ) where

import PGM.Elim
import PGM.Factor
import PGM.Vars
import PGM.Statistics


x, x1, y, z, u :: RandVarExpr
x = TopLevel "x" [(-1, 0.5), (1, 0.5)]
x1 = TopLevel "x1" [(-1, 0.5), (1, 0.5)]
y = x + x1
z = x * x1
u = abs $ y + 1

-- Make the graph out of any variables you are interested in.
-- If x is a parent of y, then you only need to include y
g :: Context
g = mkContextFromRVEs [y, z, u]

yOrd, zOrd, uOrd :: [RandVar]
yOrd = maxCardElim g [mkRV y]
zOrd = maxCardElim g [mkRV z]
uOrd = maxCardElim g [mkRV u]

yFac, zFac, uFac :: Factor
yFac = sumProdVE yOrd g
zFac = sumProdVE zOrd g
uFac = sumProdVE uOrd g

main :: IO ()
main = do
  print $ mkFactor x
  print $ mkFactor x1
  print yFac
  print zFac
  print uFac
  putStrLn $ "E[" ++ (show u) ++ "] = " ++ (show $ expectation g (mkRV u))
