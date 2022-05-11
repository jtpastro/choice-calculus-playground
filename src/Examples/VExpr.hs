module Examples.VExpr (Expr (Lit, Add, VExpr), eval, veval, evolution) where

import CC.ChoiceCalculus (V (Chc, Dim, Obj), liftV)
import qualified Data.Maybe
import Data.List ((\\))

type VExpr = V Expr

data Expr
  = Lit Int
  | Add Expr Expr
  | VExpr VExpr
  deriving (Eq)

instance Show Expr where
  show (Lit int) = show int
  show (Add e1 e2) = show e1 <> " + " <> show e2
  show (VExpr vexpr) = show vexpr

eval :: Expr -> V Int
eval (Lit i) = Obj i
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (VExpr ve) = ve >>= eval

veval :: VExpr -> V Int
veval = liftV eval

type Cache = [(Expr, V Int)]

updateCache :: (Expr, V Int) -> Cache -> Cache
updateCache (e', i') [] = [(e', i')]
updateCache (e', i') ((e, i) : cs) = if e == e' then (e', i') : cs else (e, i) : updateCache (e', i') cs

inCache :: Expr -> Cache -> Maybe (V Int)
inCache _ [] = Nothing
inCache e' ((e, i) : cs) = if e == e' then Just i else e' `inCache` cs

type PreOrderLocation = Int

type ParticularEvolution = Expr -> Cache -> PreOrderLocation -> Expr -> (Expr, Cache, V Int)

evolution :: ParticularEvolution
evolution expI cache loc nexp = let (e,_,c) = evolAux expI loc nexp cache
                                in (e,c, snd $ last c)

evolAux :: Expr -> PreOrderLocation -> Expr -> Cache -> (Expr, Int, Cache)
evolAux expI loc nexp cache = case (compare loc 0, expI) of
  (LT, _) -> error $ "Error on search: loc '" <> show loc <> "' is less than zero."
  (EQ, _) -> let (_, _, ncache)  = evolAux nexp (exprLength nexp) (Lit 0) cache in (nexp, 0, ncache )
  (GT, l@(Lit v)) -> (l, loc, cache)
  (GT, Add left right) ->
    let (leftWalk, nLoc, cache') = evolAux left (loc -1) nexp cache
     in if nLoc == 0
          then let newNode = Add leftWalk right
                   newCacheEntry = (newNode, eval'' cache' newNode)
               in (newNode, 0, updateCache newCacheEntry cache')
          else
            let (rightWalk, rnLoc, cache'') = evolAux right (nLoc - 1) nexp cache'
                newNode = Add left rightWalk 
                newCacheEntry = (newNode, eval'' cache'' newNode)
            in (newNode, rnLoc, updateCache newCacheEntry cache'')
  (GT, VExpr (Obj e)) -> let (e', nLoc, cache) = evolAux e loc nexp cache in (VExpr (Obj e'), nLoc, cache)
  (GT, VExpr (Dim d t v)) -> let (e', nLoc, cache) = evolAux (VExpr v) loc nexp cache in (VExpr (Dim d t (Obj e')), nLoc, cache)
  (GT, VExpr (Chc d [])) -> error $ "Error on search: loc '" <> show loc <> "' is greater than zero, but there are no more subexpressions to search."
  (GT, VExpr (Chc d (c:cs))) -> let (c', nLoc, cache) = evolAux (VExpr c) (loc-1) nexp cache in if nLoc == 0
          then (VExpr (Chc d (Obj c':cs)), 0, cache)
          else let (c'', nLoc'', cache) = evolAux (VExpr (Chc d cs)) (loc-1) nexp cache in (VExpr (Chc d (Obj c'':cs)), nLoc'', cache)

exprLength :: Expr -> Int
exprLength exp = length $ words $ show exp

eval'' :: Cache -> Expr -> V Int
eval'' c e@(Lit v) = Data.Maybe.fromMaybe (Obj v) (e `inCache` c)
eval'' c e@(Add e1 e2) = case e `inCache` c of
  Just i -> i
  Nothing -> (+) <$> eval e1 <*> eval e2
eval'' c e@(VExpr ve) = case e `inCache` c of
  Just i -> i
  Nothing -> veval ve
