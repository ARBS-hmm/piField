module Tree.MSO.Nest

data State = NotInCut | InCut | Root | Fail

data Labels = SumLabel | ProdLabel | NumLabel | RootLabel

{--
φ = ∃C. (∀x. Sum(x) → x ∈ C) ∧
        (∀y. Prod(y) → y ∉ C) ∧
        (∀x,y. (x ∈ C ∧ Ancestors(y,x)) → y ∈ C) ∧
        (∀z. Leaf(z) ∧ ¬Sum(z) ∧ ¬Prod(z) → z ∉ C)
--}

resolve : Labels -> State -> State -> State
resolve RootLabel Fail _ = Fail
resolve RootLabel _ Fail = Fail
resolve RootLabel Fail Fail = Fail
resolve RootLabel _ _ = Root

resolve SumLabel NotInCut NotInCut = InCut
resolve SumLabel NotInCut InCut    = InCut
resolve SumLabel InCut    NotInCut = InCut
resolve SumLabel InCut    InCut    = InCut
resolve SumLabel _        _        = Fail

-- Prod: only valid if both children are NotInCut
resolve ProdLabel NotInCut NotInCut = NotInCut
resolve ProdLabel _        _        = Fail

resolve NumLabel NotInCut NotInCut = NotInCut
resolve NumLabel NotInCut InCut    = InCut
resolve NumLabel InCut    NotInCut = InCut
resolve NumLabel InCut    InCut    = InCut
resolve NumLabel _        _        = Fail

data Expr : State -> Type where
  RootIt : {x : State} -> Expr x -> Expr (resolve RootLabel x InCut) --placeholder redundant
  Num : Nat -> Expr NotInCut
  Sum : {x, y : State} -> Expr x -> Expr y -> Expr (resolve SumLabel x y)
  Prod : {x, y : State} -> Expr x -> Expr y -> Expr (resolve ProdLabel x y)

ex1 : Expr Root
ex1 = RootIt (Sum (Num 5) (Num 3))

ex2 = RootIt (Num 9)

ex3 : Expr Fail

ex_fail1 : Expr Fail
ex_fail1 = Prod (Sum (Num 1) (Num 2)) (Num 3)

ex_fail2 : Expr Fail
ex_fail2 = Prod (Num 1) (Sum (Num 2) (Num 3))

ex_fail3 : Expr Fail
ex_fail3 = Prod (Sum (Num 1) (Num 2)) (Sum (Num 3) (Num 4))

ex_fail4 : Expr Fail
ex_fail4 = Prod (Prod (Sum (Num 1) (Num 2)) (Num 3)) (Num 4)

ex_fail5 : Expr Fail
ex_fail5 = Prod (Num 1) (Prod (Sum (Num 2) (Num 3)) (Num 4))
