module Main (main) where

import CC.ChoiceCalculus (Dim, V (Chc, Dim, Obj), semantics)
import Examples.VExpr (Expr (Lit, Add, VExpr), eval, evolution)
import CC.Edit (hoist)
import Examples.Menu (menu, Food)
import Data.Data (Data)
import qualified Examples.VList
import Data.List (sort)

e1 :: Expr
e1 = Add (Lit 1) (Lit 2)

e2 :: Expr
e2 = Add (Lit 3) (Lit 4)

e3 :: Expr
e3 =
  VExpr
    ( Dim
        "Expr"
        ["e1", "e2"]
        (Chc "Expr" [Obj e1, Obj e2])
    )

e4 :: Expr
e4 =
  VExpr
    ( Dim
        "Op"
        ["Add1", "Add2"]
        (Chc "Op" [Obj (Add e3 (Lit 1)), Obj (Add e3 (Lit 2))])
    )

e5 = Add (Add (Lit 3) (Add (Lit 2) (Lit 3))) (Add (Lit 2) (Lit 1)) 

main :: IO ()
main = do
  putStrLn "Expressions"
--  let e = Add e1 e2 in i
  let (e6,cache, result) = evolution (Lit 0) [] 0 e5
  print cache
  print $ evolution e5 cache 6 e2
  let e = Add e1 e2 in print $ evolution (Lit 0) [] 0 e
