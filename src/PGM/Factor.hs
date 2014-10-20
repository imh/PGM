{-# OPTIONS_GHC -Wall #-}

module PGM.Factor where

import PGM.Vars
import qualified Data.Map.Strict as M


data Asgn a b = a := b
instance (Show a, Show b) => Show (Asgn a b) where
  show (a := b) = (show a) ++ " := " ++ (show b)
instance (Eq a, Eq b) => Eq (Asgn a b) where
  (aVar := aVal) == (bVar := bVal) = aVar == bVar && aVal == bVal

-- pseudo probabilty, depending on the vars, can be a probability or a conditional probability, or fully general factor (as in Koller & Friedman)
--               vars             val vector-> pseudo probability
-- TODO generalize Val
data FFun = Table [([Asgn RandVar Val], Double)]
toTable :: FFun -> [([Asgn RandVar Val], Double)]
toTable (Table t) = t

data IdentifiedFactor = IF Ident Factor
instance Eq IdentifiedFactor where
  (IF i1 _) == (IF i2 _) = i1 == i2
instance Show IdentifiedFactor where
    show (IF vs _) = show vs

data Factor = F [RandVar] FFun
fFun :: Factor -> FFun
fFun (F _ fun) = fun

-- | Checks whether a RandVar is contained in the scope of a Factor
scopeContains :: RandVar -- ^ The variable to be looked up
              -> Factor         -- ^ The factor that may contain the variable
              -> Bool           -- ^ Whether the factor contains the variable
scopeContains var (F vars _) = elem var vars

fProduct :: [Factor] -> Factor
fProduct [] = F [] $ Table []
fProduct [x] = x
fProduct (x1:xs) =
  fProd x1 $ fProduct xs
  where
    fProd (F xVars xFun) (F yVars yFun) =
      F (union xVars yVars)
        (funCombine xFun yFun)

funCombine :: FFun -> FFun -> FFun
funCombine (Table xs) (Table ys) =
  Table [(union xAsgn yAsgn, xProb * yProb) | (xAsgn, xProb) <- xs,
                                              (yAsgn, yProb) <- ys,
                                              assignmentsCompatible xAsgn yAsgn]

assignmentsCompatible :: [Asgn RandVar Val] -> [Asgn RandVar Val] -> Bool
assignmentsCompatible xs ys =
  all valsMatch [(xVal, yVal) | xVar := xVal <- xs,
                                yVar := yVal <- ys,
                                xVar == yVar]

valsMatch :: (Val, Val) -> Bool
valsMatch (x, y) = x == y

union :: Eq a => [a] -> [a] -> [a]
union xs ys = [y  | y  <- ys, not $ elem y xs] ++ ys

marginalize :: RandVar -> Factor -> Factor
marginalize arg (F vars fun) =
  F (filter (not . (== arg)) vars)
    (Table $ fromPairs $
      M.toList $ M.foldrWithKey step M.empty $ M.fromList $ toPairs $ toTable $ fun)
  where
   fromPairs = (map (\(a,b) -> (map (\(x,y) -> x := y) a,b)))
   toPairs = (map (\(a,b) -> (map (\(x := y) -> (x,y)) a,b)))
   step as c acc =
     let as' = filter (not . (== arg) . fst) as
     in
      M.insertWith (+) as' c acc

-- | Drops the Ident from an IdentifiedFactor to make it a Factor
dropID :: IdentifiedFactor -> Factor
dropID (IF _ f) = f

-- |Creates a Factor that is another factor marginalized over a given variable (or set of variables?)
--marginalize :: RandVar -> Factor -> Factor
--marginalize v f = if scopeContains v f
--  then undefined
--  else error "Cannot marginalize over a function without var in scope."
makeIFactor :: RandVarExpr -> IdentifiedFactor
makeIFactor v@(TopLevel _ list) = IF v $ F [vVar] $
                                    Table (map (\x -> ([vVar := (fst x)],
                                                       snd x))
                                               list)
                                  where
                                    vVar = makeRV v
makeIFactor v@(Const x)         = IF v $ F [vVar] $
                                    Table [([vVar := x],
                                            1)]
                                  where
                                    vVar = makeRV v
makeIFactor v@(Add x y)         = IF v $ F [xVar, yVar, vVar] $
                                    Table [([xVar := xVal,
                                             yVar := yVal,
                                             vVar := (xVal + yVal)],
                                            1) | xVal <- rvVals xVar,
                                                 yVal <- rvVals yVar]
                                  where
                                    xVar = makeRV x
                                    yVar = makeRV y
                                    vVar = makeRV v
makeIFactor v@(Mul x y)         = IF v $ F [xVar, yVar, vVar] $
                                    Table [([xVar := xVal,
                                             yVar := yVal,
                                             vVar := (xVal * yVal)],
                                            1) | xVal <- rvVals xVar,
                                                 yVal <- rvVals yVar]
                                  where
                                    xVar = makeRV x
                                    yVar = makeRV y
                                    vVar = makeRV v
makeIFactor v@(Abs x)           = IF v $ F [xVar, vVar] $
                                    Table [([xVar := xVal,
                                             vVar := (abs xVal)],
                                            1) | xVal <- rvVals xVar]
                                  where
                                    xVar = makeRV x
                                    vVar = makeRV v
makeIFactor v@(Signum x)        = IF v $ F [xVar, vVar] $
                                    Table [([xVar := xVal,
                                             vVar := (signum xVal)],
                                            1) | xVal <- rvVals xVar]
                                  where
                                    xVar = makeRV x
                                    vVar = makeRV v
