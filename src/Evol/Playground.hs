module Evol.Playground where

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

{- If the expression evolves, do we have to evaluate it from scratch?
   or is it possible to reuse the evaluation on parts of the initial expression?
   (we refer to the latter strategy as 'evolution-aware')
-}

{-
 Recalling state-of-the-practice development, after model (code) evoluiton,
 not all model (code) base is analyzed again from scratch. For instance, this
 happens in compilation and often in regression testing. So ideally, we
 would like to have the following evalIdeal function, which is evolution-aware
-}

type Cache = [(Expr, Int)]

updateCache :: (Expr, Int) -> Cache -> Cache
updateCache (e', i') [] = [(e', i')]
updateCache (e', i') ((e, i) : cs) = if e == e' then (e', i') : cs else (e, i) : updateCache (e', i') cs

inCache :: Expr -> Cache -> Maybe Int
inCache _ [] = Nothing
inCache e' ((e, i) : cs) = if e == e' then Just i else e' `inCache` cs

evalIdeal :: Expr -> Cache -> Expr -> Int
evalIdeal = undefined

{-
 where the first parameter is an initial expression that is assumed to be evaluated having  its
 intermediate results provided in a cache (second parameter). The second expression
(third parameter) is the evolved expression. The job of evalIdeal is then to evaluate this
new expression trying to reuse as much as possible of the intermediate results
stored in the cache. In general, such reuse will depend on how the evolution
happens.  For the sake of simplicity, in this project we will make some simplifications
(shared by some state-of-the-art research) restricting the evoluiton
-}

type PreOrderLocation = Int

type ParticularEvolution = Expr -> PreOrderLocation -> Expr -> Expr

evolution :: ParticularEvolution
evolution expI loc nexp = fst (evolAux expI loc nexp)

evolAux :: Expr -> PreOrderLocation -> Expr -> (Expr, Int)
evolAux expI loc nexp
  | loc == 0 = (nexp, 0)
  | loc > 0 = case expI of
    Lit _ -> (expI, loc -1)
    Add left right ->
      let (leftWalk, nLoc) = evolAux left (loc -1) nexp
       in if (nLoc == 0)
            then (Add leftWalk right, 0)
            else
              let (rightWalk, rnLoc) = evolAux right nLoc nexp
               in (Add left rightWalk, rnLoc)

evalEA :: Expr -> Cache -> ParticularEvolution -> PreOrderLocation -> Expr -> (Expr -> Int)
evalEA expI cache evolution loc nexp =
  let evolvedModel = evolution expI loc nexp
      analysisLocalChange = eval nexp
      newCache = updateCache (nexp, analysisLocalChange) cache
   in eval'' newCache

eval'' :: Cache -> Expr -> Int
eval'' c e@(Lit _) = case e `inCache` c of
  Just i -> i
  Nothing -> eval e
eval'' c e@(Add e1 e2) = case e `inCache` c of
  Just i -> i
  Nothing -> eval'' c e1 + eval'' c e2

{-
Data.Map
evol :: Expr          -> Int ->   Expr  ->  Expr
evol  (1+4) * (2 + 3)     7      (3-4)      (1+4) * (2 + (3-4))

(1+4) * (2 + 3)
(1+4) * (2 + (3-4))

------------
evol  3 =  3-4

eval ordinary Expr ----> evolutoion-aware eval J.

---------------------------
-- **** Possible extensions
-- * explore the feasibility of leveraging zippers to implement the evolution function
-- * use memoization when analyzing the original model to infer the cache
-- * optimize eval'' trying to leverage as much as possible the cache, which might evolve from subexpressions
-- * address other forms of evolution (e.g., removal of subexpression, changing subexpresion, etc.)
---------------------------

evol :: VExpr -> Int (Chc / Dim ??) -> VExpr -> VExpr

evalL :: VExpr -> V Int

eval' :: VExpr -> evol... -> Cache -> V Int

eval :: Expr -> V Int
eval (Lit i) = Obj i
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (VExpr ve) = ve >>= eval

veval :: VExpr -> V Int
veval = liftV eval

(1+4) * (2 + 3)
(1+4) * (2 + (3-4))

(1+4) * ( #ifdef f1
           2
          #elif
           20
                   +
        3)

... Dim ... ( 1 +  Dim Chc <4,4> }) * (2 + 3)
(1+4) * (2 + (3-4))

e4 :: Expr
e4 =
  VExpr
    ( Dim
        "Op"
        ["Add1", "Sub2"]
        (Chc "Op" [Obj (Add e3 (Lit 1)), Obj (Sub e3 (Lit 2))])
    )

-}