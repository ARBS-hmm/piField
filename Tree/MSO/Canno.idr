module Tree.MSO.Canno

data State = P | Np | Fail | RootS

data Labels = SumLabel | ProdLabel | NumLabel | RootLabel

{--
φ = ∀x. (Sum(x) ∧ ∃y. (LeftChild(y,x) ∧ Prod(y))) → 
         ∃z. (RightChild(z,x) ∧ Prod(z))
--}

resolve : State -> State -> State
resolve P Np = Fail
resolve P P = Np
resolve Fail _ = Fail
resolve _ Fail = Fail
resolve Np _ = Np
resolve _ Np = Np
resolve _ RootS = Fail 
resolve RootS _ = Fail

check : State -> State
check Fail = Fail 
check _ = RootS

data Expr : State -> Type where
  Root : {x : State} -> Expr x -> Expr (check x)
  Num : Nat -> Expr Np
  Sum : {x, y : State} -> Expr x -> Expr y -> Expr (resolve x y)
  Prod : {x, y : State} -> Expr x -> Expr y -> Expr P

Show State where
  show P = "P"
  show Np = "Np"
  show Fail = "Fail"
  show RootS = "RootS"

Show (Expr s) where
  show (Num n) = show n
  show (Sum l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Prod l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
  show (Root e) = "Root(" ++ show e ++ ")"

stateOf : Expr s -> State
stateOf (Num _) = Np
stateOf (Sum {x, y} _ _) = resolve x y
stateOf (Prod _ _) = P
stateOf (Root {x} _) = check x

showExpr : Expr s -> String
showExpr e = show e ++ " : " ++ show (stateOf e)

ex1 : Expr RootS
ex1 = Root (Sum (Num 1) (Num 2))

ex2 : Expr RootS
ex2 = Root (Sum (Prod (Num 1) (Num 2)) (Prod (Num 3) (Num 4)))

ex3 : Expr RootS
ex3 = Root (Prod (Sum (Num 1) (Num 2)) (Num 3))

ex4 : Expr RootS
ex4 = Root (Sum (Num 1) (Prod (Sum (Num 2) (Num 3)) (Num 4)))

ex5 : Expr Fail
ex5 = Root (Sum (Prod (Num 1) (Num 2)) (Num 3))

ex6 : Expr RootS
ex6 = Root (Num 5)

ex7 : Expr Fail
ex7 = Root (Sum (Prod (Num 1) (Num 2)) (Sum (Num 3) (Num 4)))

ex8 : Expr RootS
ex8 = Root (Sum (Num 1) (Prod (Num 2) (Num 3)))

ex9 : Expr RootS
ex9 = Root (Sum (Num 1) (Num 2))

ex10 : Expr RootS
ex10 = Root (Prod (Num 1) (Prod (Num 2) (Num 3)))

ex11 : Expr RootS
ex11 = Root (Sum (Sum (Num 1) (Num 2)) (Sum (Num 3) (Num 4)))

ex12 : Expr RootS
ex12 = Root (Sum (Sum (Num 1) (Num 2)) (Prod (Num 3) (Num 4)))

ex13 : Expr Fail
ex13 = Root (Sum (Prod (Num 1) (Prod (Num 2) (Num 3))) (Num 4))

ex14 : Expr RootS
ex14 = Root (Prod (Sum (Num 1) (Num 2)) (Sum (Num 3) (Num 4)))

ex15 : Expr RootS
ex15 = Root (Sum (Prod (Num 1) (Num 2)) (Prod (Num 3) (Sum (Num 4) (Num 5))))

ex16 : Expr RootS
ex16 = Root (Sum (Num 1) (Sum (Num 2) (Num 3)))

ex17 : Expr RootS
ex17 = Root (Prod (Prod (Num 1) (Num 2)) (Prod (Num 3) (Num 4)))

ex18 : Expr RootS
ex18 = Root (Sum (Prod (Num 1) (Num 2)) (Prod (Prod (Num 3) (Num 4)) (Num 5)))

ex19 : Expr Fail
ex19 = Root (Sum (Prod (Num 1) (Num 2)) (Num 3))

ex20 : Expr RootS
ex20 = Root (Sum (Prod (Num 1) (Num 2)) (Prod (Num 3) (Num 4)))

main : IO ()
main = do
  putStrLn "=== Tree Automaton Expressions ==="
  putStrLn ""
  putStrLn $ "1.  " ++ showExpr ex1
  putStrLn $ "2.  " ++ showExpr ex2
  putStrLn $ "3.  " ++ showExpr ex3
  putStrLn $ "4.  " ++ showExpr ex4
  putStrLn $ "5.  " ++ showExpr ex5
  putStrLn $ "6.  " ++ showExpr ex6
  putStrLn $ "7.  " ++ showExpr ex7
  putStrLn $ "8.  " ++ showExpr ex8
  putStrLn $ "9.  " ++ showExpr ex9
  putStrLn $ "10. " ++ showExpr ex10
  putStrLn $ "11. " ++ showExpr ex11
  putStrLn $ "12. " ++ showExpr ex12
  putStrLn $ "13. " ++ showExpr ex13
  putStrLn $ "14. " ++ showExpr ex14
  putStrLn $ "15. " ++ showExpr ex15
  putStrLn $ "16. " ++ showExpr ex16
  putStrLn $ "17. " ++ showExpr ex17
  putStrLn $ "18. " ++ showExpr ex18
  putStrLn $ "19. " ++ showExpr ex19
  putStrLn $ "20. " ++ showExpr ex20
