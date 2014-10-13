{-# OPTIONS_GHC -Wall #-}

module PGM.Factor where

import PGM.Vars
import Control.Applicative ((<$>), (<*>))
import qualified Data.Set as Set

-- pseudo probabilty, depending on the vars, can be a probability or a conditional probability, or fully general factor (as in Koller & Friedman)
--               vars             val vector-> pseudo probability
-- TODO generalize away from Rational
data FFun = Fun ([Rational] -> Maybe Double)
--          | Table [([Rational], Double)]
getFun :: FFun -> ([Rational] -> Maybe Double)
getFun (Fun f) = f


-- TODO generalize beyond Rational
data IdentifiedFactor = IF Ident Factor
instance Eq IdentifiedFactor where
  (IF i1 _) == (IF i2 _) = i1 == i2
instance Show IdentifiedFactor where
    show (IF vs _) = show vs

-- TODO generalize beyond Rational
data Factor = F [RandomVariable] FFun
fFun :: Factor -> FFun
fFun (F _ fun) = fun

-- | Checks whether a RandomVariable is contained in the scope of a Factor
scopeContains :: RandomVariable -- ^ The variable to be looked up
              -> Factor         -- ^ The factor that may contain the variable
              -> Bool           -- ^ Whether the factor contains the variable
scopeContains v (F vs _) = elem v vs

-- |Constructs a function [Rational] -> [Rational] that we can use to
-- call functions defined over domain (X :: [Rational]) as if they were defined
-- over domain (Y :: [Rational]).
-- argConv dom1 dom2 = (vals1 -> vals2)
argConv :: [RandomVariable]         -- ^ The original domain
        -> [RandomVariable]         -- ^ The new domain
        -> [Rational] -> [Rational] -- ^ A function from values in the original
                                    --   domainto values in the new domain
argConv rsOld rsNew = project $ reIndex rsOld rsNew
-- this feels like it should be pointfree, but it's a weird partial application. Ideas?


-- |Projects out onto certain indices from a list of rationals
project :: [Int]      -- ^ The indices of the rationals to keep
        -> [Rational] -- ^ The rationals to project from
        -> [Rational] -- ^ The rationals left over
project inds values = _project inds values [] where
  _project [] _ pvals = pvals
  _project (ind:_inds) _values pvals =
    _project _inds _values (pvals ++ [_values !! ind])

-- |Makes a list of the indices of a RandomVariable as present in another list
-- of RandomVariables
reIndex :: [RandomVariable] -- ^ The RandomVariables to do the lookup in
        -> [RandomVariable] -- ^ The RandomVariables to lookup
        -> [Int]            -- ^ The indices of the second list in the first
reIndex rsOld rsNew = map (indexIn rsOld) rsNew

-- |
indexIn :: [RandomVariable] -> RandomVariable -> Int
indexIn rs rv = _indexIn rs rv 0 where
  _indexIn [] _ _ = error "Index not found."
  _indexIn (r:_rs) _rv count = if r == _rv
                               then count
                               else _indexIn _rs _rv (count + 1)

-- | Drops the Ident from an IdentifiedFactor to make it a Factor
dropID :: IdentifiedFactor -> Factor
dropID (IF _ f) = f

-- | Gives a factor a new domain/scope.
reScope :: [RandomVariable] -- ^ The new domain
        -> Factor           -- ^ A factor to be given new scope
        -> Factor           -- ^ A factor with the same function defined over a new domain
reScope rvs (F rvsOld (Fun fun)) = F rvs $ Fun (fun . (argConv rvsOld rvs))
--reScope rvs (NIF rvsOld fun) = NIF rvs (fun $ argConv rvsOld rvs)

rvUnion :: [RandomVariable] -> [RandomVariable] -> [RandomVariable]
rvUnion rvs1 rvs2 = Set.elems $ Set.union (Set.fromList rvs1) (Set.fromList rvs2)

-- |Creates a factor that is the product of two other factors, whose scope is
-- the union of their scopes.
fProd :: Factor -> Factor -> Factor
fProd f1@(F rvs1 _) f2@(F rvs2 _) =
  F rvs_ (Fun fun_) where
    rvs_ = rvUnion rvs1 rvs2
    newf1 = (reScope rvs_ f1)
    newf2 = (reScope rvs_ f2)
    fun_ = (\vals -> (*) <$> ((getFun $ fFun newf1) vals)
                         <*> ((getFun $ fFun newf2) vals))

-- |Creates a Factor that is the product of a list of other factors, whose scope
-- is the union of their scopes
fProds :: [Factor] -> Factor
fProds [] = F [] (Fun (\_ -> Just 1))
fProds [x] = x
fProds (x:xs) = fProd x $ fProds xs

-- |Evaluates a function over a list of arguments, summing the results.
funSum :: [[Rational]] -> ([Rational] -> Double) -> Double
funSum [] _ = 0
funSum (x:xs) fun = (fun x) + (funSum xs fun)

--sepArgs :: [RandomVariable] -> RandomVariable -> [Rational] -> ([Rational], [Rational])
--sepArgs vs v rs = sepArgsRec vs v rs [] where
--  sepArgsRec [] v2 rsEnd rsFront = (rsFront, rsEnd)
--  sepArgsRec (v_:vs_) v2 [] rsFront =
--  sepArgsRec (v_:vs_) v2 (rE:rsEnd) rsFront

--margArgs :: [RandomVariable] -> RandomVariable -> [Rational] -> [[Rational]]
--margArg fullVars arg leftoverArgs


-- |Creates a Factor that is another factor marginalized over a given variable (or set of variables?)
marginalize :: RandomVariable -> Factor -> Factor
marginalize v f = if scopeContains v f
  then undefined
  else error "Cannot marginalize over a function without var in scope."

makeIFactor :: RandomVariableExpr -> IdentifiedFactor
makeIFactor v@(TopLevel _ list) = IF v $ F [makeRV v] $ Fun (\rs -> lookupProb (rs !! 0) list)
makeIFactor v@(Const x)         = IF v $ F [makeRV v] $ Fun (\rs -> lookupProb (rs !! 0 ) [(x, 1)])
makeIFactor v@(Add x y)         = IF v $ F (map makeRV [x, y, v]) $ Fun (\rs -> if (rs !! 0) + (rs !! 1) == (rs !! 2) then Just 1 else Just 0)
makeIFactor v@(Mul x y)         = IF v $ F (map makeRV [x, y, v]) $ Fun (\rs -> if (rs !! 0) * (rs !! 1) == (rs !! 2) then Just 1 else Just 0)
makeIFactor v@(Abs x)           = IF v $ F (map makeRV [x, v]) $ Fun (\rs -> if (abs (rs !! 0) == rs !! 1) then Just 1 else Just 0)
makeIFactor v@(Signum x)        = IF v $ F (map makeRV [x, v]) $ Fun (\rs -> if (signum (rs !! 0) == rs !! 1) then Just 1 else Just 0)
