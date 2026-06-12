module Lang.Hazards

%default total

data Var = A | B | C

mutual 
  data Q = Normal | CNF | DNF | Safe | PendingConsensus (Expr a)

  data Expr : Q -> Type where
    Num   : Var -> Expr Normal
    Sum   : Expr q1 -> Expr q2 -> Expr q1
    Prod  : Expr q1 -> Expr q2 -> Expr q2
    Compl : Expr q -> Expr q
