module Lang.Regex

import Data.Nat

data Pattern : Type where
  Term : Type -> Pattern
  Add  : Pattern -> Pattern -> Pattern
  Prod : Pattern -> Pattern -> Pattern
  Kleene : Pattern -> Pattern

inst : Pattern -> Nat -> Type
inst (Term t) 1 = t
inst (Term t) _ = Void
inst (Add t1 t2) n = Either (inst t1 n) (inst t2 n)
inst (Prod p1 p2) n = (k : Nat ** (inst p1 k, inst p2 (minus n k)))
inst (Kleene p) Z = Unit
inst (Kleene p) (S k) = (inst p 1, inst (Kleene p) k) 

p1 : Pattern
p1 = Kleene (Kleene (Term Int))

p2 : Pattern
p2= Prod (Add (Term Int) (Kleene (Term String)))
     (Prod (Kleene (Term Bool)) (Term Char))

t2 : inst (Prod (Add (Term Int) (Kleene (Term String)))
                (Prod (Kleene (Term Bool)) (Term Char))) 3
t2 = (2 ** (Right ("hello", ("world", ())), 
            (0 ** ((), 'a'))))

p3 : Pattern
p3 = Kleene (Add (Term Int)
        (Prod (Term String)
          (Kleene (Add (Term Bool)
                  (Prod (Term Int)
                    (Kleene (Add (Term Char)
                            (Prod (Term Nat) (Term Double)))))))))


func : (p : Pattern) -> (n : Nat) -> (inst p n) -> Type
func (Term t) 1 _ = t
func (Add p1 p2) n (Left x) = func p1 n x
func (Add p1 p2) n (Right y) = func p2 n y
func (Prod p1 p2) n (k ** (x, y)) = func p1 k x -> func p2 (minus n k) y
func (Kleene p) Z _ = Unit
func (Kleene p) (S k) (x, xs) = func p 1 x -> func (Kleene p) k xs

