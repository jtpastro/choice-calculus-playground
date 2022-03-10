module VExpr where

import ChoiceCalculus (V (Obj), liftV)

type VExpr = V Expr

data Expr
  = Lit Int
  | Add Expr Expr
  | AddTern Expr Expr Expr
  | Sub Expr Expr
  | VExpr VExpr

instance Show Expr where
  show (Lit int) = show int
  show (Add e1 e2) = show e1 <> " + " <> show e2
  show (AddTern e1 e2 e3) = show e1 <> " +t " <> show e2 <> " +t " <> show e3
  show (Sub e1 e2) = show e1 <> " - " <> show e2
  show (VExpr vexpr) = show vexpr

eval :: Expr -> V Int
eval (Lit i) = Obj i
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (AddTern e1 e2 e3) = addTern <$> eval e1 <*> eval e2 <*> eval e3
  where addTern a b c = a + b + c

eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (VExpr ve) = ve >>= eval

veval :: VExpr -> V Int
veval = liftV eval