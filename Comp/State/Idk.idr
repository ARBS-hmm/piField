module Comp.State.Idk

import Data.Fin

data State : Type where
  Start : State
  A : State
  Accept : State
  Dead : State

transition : State -> Fin 2 -> State
transition Start 0 = A
transition Start 1 = Accept
transition A 0 = A
transition A 1 = Accept
transition Accept _ = Accept
transition Dead _ = Dead

combineStates : State -> State -> State
combineStates Accept _ = Accept
combineStates _ Accept = Accept
combineStates Dead _ = Dead
combineStates _ Dead = Dead
combineStates _ _ = Start

data TreeRegex : State -> Type where
  TLeaf : (bit : Fin 2) -> TreeRegex (transition Start bit)
  TBranch : TreeRegex s1 -> TreeRegex s2 -> 
            TreeRegex (combineStates s1 s2)

-- #hmm
-- atleast one branch has a 1 
acceptTree : TreeRegex Accept
acceptTree = TBranch (TLeaf 0)
                     (TBranch (TLeaf 1)
                              (TLeaf 0))


