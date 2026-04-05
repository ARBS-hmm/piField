module Config.Shape

import Data.Fin

A : Type
A = Fin 2

data B : A -> Type where
  BZ : B 0
  BO : B 1
  BS : B 0 -> B 0

data C : (a:A) -> B (a) -> Type where
  CZ : C 0 BZ
  CS : C 0 n -> C 0 (BS n)

record RawConfig where
  constructor Raw
  a : A
  b : B a
  c : C a b

