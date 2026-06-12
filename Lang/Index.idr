module Lang.Index

import Data.Vect

data Ast : Type where
  N : Ast
  U : Ast
  Lit : Fin 2 -> Ast
  Add : Ast -> Ast -> Ast
  Mul : Ast -> Ast -> Ast

f : Ast -> Ast
norms : Ast -> Ast
normp : Ast -> Ast

data Expr : Ast -> Ast -> Type where
  Base : (t:Ast) -> Expr t (f t)
  Sum : Expr a p -> Expr b q -> Expr (Add a b) (norms (Add p q))
  Prod : Expr a p -> Expr b q -> Expr (Mul a b) (normp (Mul p q))

safe : (t:Ast**Expr t N)



