module Comp.State.Regex

-- ab(a+b)*
data State : Type where
  Start : State
  A : State
  Dead : State
  Accept : State

transition : State -> Nat -> State
transition Start 0 = A
transition A 0 = Dead
transition Start (S k) = Dead
transition A (S k) = Accept
transition Dead _ = Dead
transition Accept _ = Accept

data Regex : State -> Type where
  Nil : Regex Start
  (::) : (n:Nat) -> Regex s -> Regex (transition s n) 

x : Regex Accept
x = [3,1,0]

dead : Regex Dead
dead = [1]




