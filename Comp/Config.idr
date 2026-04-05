module Comp.Config

record Person (hasAge : Bool) (hasDOB : Bool) where
  constructor MkPerson
  name : String
  age : if hasAge then Nat else ()
  dob : if hasDOB then (Nat, Nat, Nat) else ()

-- Smart constructors
mkPersonWithAge : String -> Nat -> Person True False
mkPersonWithAge name age = MkPerson name age ()

getAge : Person True hasDOB -> Nat
getAge (MkPerson name age dob) = age

