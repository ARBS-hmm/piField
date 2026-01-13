module Comp.PolyCons

mutual
  data Fam : Bool -> Type where
    A : (n : Nat) -> Fam (P n)
  
  P : Nat -> Bool
  P 0 = False
  P (S _) = True

