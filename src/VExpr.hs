module VExpr (Expr (Lit, Add, Sub, VExpr), eval, veval) where

import ChoiceCalculus (V (Obj), liftV)

type VExpr = V Expr

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | VExpr VExpr

instance Show Expr where
  show (Lit int) = show int
  show (Add e1 e2) = show e1 <> " + " <> show e2
  show (Sub e1 e2) = show e1 <> " - " <> show e2
  show (VExpr vexpr) = show vexpr

eval :: Expr -> V Int
eval (Lit i) = Obj i
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (VExpr ve) = ve >>= eval

veval :: VExpr -> V Int
veval = liftV eval