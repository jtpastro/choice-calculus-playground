module Main (main) where

import ChoiceCalculus (V (Chc, Dim, Obj), semantics)
import VExpr (Expr (Add, Lit, Sub, VExpr), eval)

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
        ["Add1", "Sub2"]
        (Chc "Op" [Obj (Add e3 (Lit 1)), Obj (Sub e3 (Lit 2))])
    )

main :: IO ()
main = do
  print (eval e1)
  print (eval e2)
  print (eval e3)
  print (eval e4)
  print (semantics (eval e4))
