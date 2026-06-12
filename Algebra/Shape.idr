module Algebra.Shape
import Data.Vect

data Inc: (a:Type) -> Nat -> Type where
  Nil : Inc a 0
  (::) : Vect (S n) a -> Inc a n -> Inc a (S n) 


