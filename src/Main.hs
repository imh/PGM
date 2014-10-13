{-# OPTIONS_GHC -Wall #-}

module Main
       ( main
       ) where

import PGM.Factor
import PGM.Graph
import PGM.Vars

x = TopLevel "x" [(-1, 0.5), (1, 0.5)]
y = 3
z = TopLevel "z" [(-1, 0.25), (0, 0.5), (1, 0.5)]
a = x + y
b = a * z
c = b * x

main :: IO ()
main = do
  putStrLn $ show c

--main :: IO ()
--main = do
--  x <- return $ TopLevel "x" [(-1, 0.5), (1, 0.5)]
--  y <- return $ Const 3
--  z <- return $ TopLevel "z" [(-1, 0.25), (0, 0.5), (1, 0.5)]
--  a <- return $ Add x y
--  b <- return $ Mul a z
--  c <- return $ Mul b x
--  putStrLn $ show c