module Comp.ParityBlock

data Parity : Type where
  Odd : Parity
  Even : Parity

flip : Parity -> Parity
flip Odd = Even
flip Even = Odd

data ParityBlock : Parity -> Type -> Type where
  Nil : ParityBlock Even a
  (::) : a -> ParityBlock p a -> ParityBlock (flip p) a

odd : ParityBlock Odd Nat
odd = [1,2,3]

even : (ParityBlock Even Nat)
even = [1,2]

  

