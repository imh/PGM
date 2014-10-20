{-# OPTIONS_GHC -Wall #-}

module PGM.Factor where

import PGM.Vars
import qualified Data.Map.Strict as M


newtype Asgn a b = Asgn (a, b)
instance (Show a, Show b) => Show (Asgn a b) where
  show (Asgn (a, b)) = (show a) ++ " #= " ++ (show b)
instance (Eq a, Eq b) => Eq (Asgn a b) where
  (Asgn (avar,aval)) == (Asgn (bvar, bval)) = avar == bvar && aval == bval

(#=) :: (Show a, Show b) => a -> b -> Asgn a b
a #= b = Asgn (a, b)

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
scopeContains v (F vs _) = elem v vs


fProduct :: [Factor] -> Factor
fProduct [] = F [] $ Table []
fProduct [x] = x
fProduct (x1:xs) =
  fProd x1 $ fProduct xs
  where
    fProd (F xvs xf) (F yvs yf) =
      F (union xvs yvs)
        (funCombine xf yf)

funCombine :: FFun -> FFun -> FFun
funCombine (Table xs) (Table ys) =
  Table [(union x y, xp * yp) | (x,xp) <- xs,
                                (y,yp) <- ys,
                                assignmentsCompatible x y]

assignmentsCompatible :: [Asgn RandVar Val] -> [Asgn RandVar Val] -> Bool
assignmentsCompatible xs ys =
  all valsMatch [(xval, yval) | Asgn (xvar, xval) <- xs,
                                Asgn (yvar, yval) <- ys,
                                xvar == yvar]

valsMatch :: (Val, Val) -> Bool
valsMatch (x, y) = x == y

union :: Eq a => [a] -> [a] -> [a]
union xs ys = [y  | y  <- ys, not $ elem y xs] ++ ys

marginalize :: RandVar -> Factor -> Factor
marginalize arg (F vs fun) =
  F (filter (not . (== arg)) vs)
    (Table $ fromPairs $
      M.toList $ M.foldrWithKey step M.empty $ M.fromList $ toPairs $ toTable $ fun)
  where
   fromPairs = (map (\(a,b) -> (map (\(x,y) -> x #= y) a,b)))
   toPairs = (map (\(a,b) -> (map (\(Asgn (x,y)) -> (x,y)) a,b)))
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
makeIFactor v@(TopLevel _ list) = IF v $ F [rvv] $
                                    Table (map (\x -> ([rvv #= (fst x)],
                                                       snd x))
                                               list)
                                  where
                                    rvv = makeRV v
makeIFactor v@(Const x)         = IF v $ F [rvv] $
                                    Table [([rvv #= x],
                                            1)]
                                  where
                                    rvv = makeRV v
makeIFactor v@(Add x y)         = IF v $ F [rvx, rvy, rvv] $
                                    Table [([rvx #= xv,
                                             rvy #= yv,
                                             rvv #= (xv + yv)],
                                            1) | xv <- rvVals rvx,
                                                 yv <- rvVals rvy]
                                  where
                                    rvx = makeRV x
                                    rvy = makeRV y
                                    rvv = makeRV v
makeIFactor v@(Mul x y)         = IF v $ F [rvx, rvy, rvv] $
                                    Table [([rvx #= xv,
                                             rvy #= yv,
                                             rvv #= (xv * yv)],
                                            1) | xv <- rvVals rvx,
                                                 yv <- rvVals rvy]
                                  where
                                    rvx = makeRV x
                                    rvy = makeRV y
                                    rvv = makeRV v
makeIFactor v@(Abs x)           = IF v $ F [rvx, rvv] $
                                    Table [([rvx #= xv,
                                             rvv #= (abs xv)],
                                            1) | xv <- rvVals rvx]
                                  where
                                    rvx = makeRV x
                                    rvv = makeRV v
makeIFactor v@(Signum x)        = IF v $ F [rvx, rvv] $
                                    Table [([rvx #= xv,
                                             rvv #= (signum xv)],
                                            1) | xv <- rvVals rvx]
                                  where
                                    rvx = makeRV x
                                    rvv = makeRV v
