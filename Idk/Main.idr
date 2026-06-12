module Idk.Main

data Grammar : Type where
  A_G : Grammar
  B_G : Grammar
  Kleene_G : Grammar -> Grammar
  Sum_G : Grammar -> Grammar -> Grammar
  Prod_G : Grammar -> Grammar -> Grammar

data Regex :Type where
  A : Regex
  B : Regex
  Kleene : Regex -> Regex
  Sum : Regex -> Regex -> Regex
  Prod : Regex -> Regex -> Regex

fam : Grammar -> Regex
fam A_G = A
fam B_G = B
fam (Kleene_G r) = (Kleene (fam r))
fam (Sum_G r r') = Sum (fam r) (fam r')
fam (Prod_G r r') = Prod (fam r) (fam r')



