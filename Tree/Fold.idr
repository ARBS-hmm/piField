module Tree.Fold
%default total

data Status = Number | Variable | Mix

Show Status where
  show Number = "Number"
  show Variable = "Variable"
  show Mix = "Mix"

data V = X | Y | Z

Show V where
  show X = "x"
  show Y = "y"
  show Z = "z"

resolve : Status -> Status -> Status
resolve Number Number = Number
resolve Number Variable = Mix
resolve Variable Number = Mix
resolve Variable Variable = Variable
resolve Mix _ = Mix
resolve _ Mix = Mix

data Alg : Status -> Type where
  Num  : Nat -> Alg Number
  Var  : V -> Alg Variable
  Sum  : {x,y : Status} -> Alg x -> Alg y -> Alg (resolve x y)
  Prod : {x,y : Status} -> Alg x -> Alg y -> Alg (resolve x y)

Show (Alg s) where
  show (Num n) = show n
  show (Var v) = show v
  show (Sum {x} {y} a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Prod {x} {y} a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

fold : Alg Number -> Nat
fold (Num n) = n
fold (Sum {x=Number} {y=Number} a b) = fold a + fold b
fold (Prod {x=Number} {y=Number} a b) = (fold a)*(fold b)
fold _ = 0 --never hit

route : (x,y : Status) -> (a : Alg x) -> (b : Alg y) -> 
        (Nat -> Nat -> Nat) -> (cons : Alg x -> Alg y -> Alg (resolve x y)) ->
        (t : Status ** Alg t)
route Number Number a b op cons = (Number ** Num (op (fold a) (fold b)))
route x y a b op cons = (resolve x y ** cons a b)

normalize : (s : Status) -> Alg s -> (t : Status ** Alg t)
normalize Number (Num n) = (Number ** (Num n))
normalize Variable (Var v) = (Variable ** Var v)
normalize (resolve x y) (Sum {x} {y} a b) = 
  let (x' ** a') = normalize x a
      (y' ** b') = normalize y b
  in route x' y' a' b' (+) Sum
normalize (resolve x y) (Prod {x} {y} a b) = 
  let (x' ** a') = normalize x a
      (y' ** b') = normalize y b
  in route x' y' a' b' (*) Prod

ex1 : Alg Number
ex1 = Prod (Sum (Num 5) (Num 3)) (Sum (Num 2) (Num 4))

ex2 : Alg Mix
ex2 = Sum (Prod (Num 2) (Num 3)) (Var X)

ex3 : Alg Mix
ex3 = Prod (Sum (Num 5) (Num 3)) (Sum (Var X) (Num 2))

ex4 : Alg Variable
ex4 = Prod (Var X) (Var Y)

main : IO ()
main = do
  putStrLn ""
  putStrLn "Example 1: Pure numbers - should fully evaluate to a single number"
  putStrLn ("Original: " ++ show ex1)
  let (s1 ** n1) = normalize Number ex1
  putStrLn ("Normalized: " ++ show n1 ++ " (Status: " ++ show s1 ++ ")")
  putStrLn ""
  
  putStrLn "Example 2: Numbers with one variable - should partially evaluate"
  putStrLn ("Original: " ++ show ex2)
  let (s2 ** n2) = normalize Mix ex2
  putStrLn ("Normalized: " ++ show n2 ++ " (Status: " ++ show s2 ++ ")")
  putStrLn ""
  
  putStrLn "Example 3: Mixed with nested operations - should evaluate all numeric parts"
  putStrLn ("Original: " ++ show ex3)
  let (s3 ** n3) = normalize Mix ex3
  putStrLn ("Normalized: " ++ show n3 ++ " (Status: " ++ show s3 ++ ")")
  putStrLn ""
  
  putStrLn "Example 4: All variables - should stay as Variable"
  putStrLn ("Original: " ++ show ex4)
  let (s4 ** n4) = normalize Variable ex4
  putStrLn ("Normalized: " ++ show n4 ++ " (Status: " ++ show s4 ++ ")")
  putStrLn ""
  
  pure()
