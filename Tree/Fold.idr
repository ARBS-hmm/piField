module Tree.Fold

import Data.Nat

data Status = Foldable | Mix | NFold

Show Status where
  show Foldable = "Foldable"
  show Mix = "Mix"
  show NFold = "NFold"

data V = X | Y | Z

Show V where
  show X = "x"
  show Y = "y"
  show Z = "z"

resolve : Status -> Status -> Status
resolve Foldable Foldable = Foldable
resolve Foldable NFold = Mix
resolve NFold Foldable = Mix
resolve NFold NFold = NFold
resolve Mix _ = Mix
resolve _ Mix = Mix

data Alg : Status -> Type where
  Num  : Nat -> Alg Foldable
  Var  : V -> Alg NFold
  Sum  : {x,y : Status} -> Alg x -> Alg y -> Alg (resolve x y)
  Prod : {x,y : Status} -> Alg x -> Alg y -> Alg (resolve x y)

Show (Alg s) where
  show (Num n) = show n
  show (Var v) = show v
  show (Sum {x} {y} a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Prod {x} {y} a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

fold : Alg s -> Nat
fold (Num n) = n
fold (Sum a b) = fold a + fold b
fold (Prod a b) = fold a * fold b
fold (Var a) = 0

normalize : (s : Status) -> Alg s -> (t : Status ** Alg t)
normalize Foldable (Num n) = (Foldable ** (Num n))
normalize NFold (Var v) = (NFold ** Var v)
normalize (resolve x y) (Sum {x} {y} a b) = 
  let (x' ** a') = normalize x a
      (y' ** b') = normalize y b
  in routeSum x' y' a' b'
  where
    routeSum : (x,y : Status) -> (a : Alg x) -> (b : Alg y) -> (t : Status ** Alg t)
    routeSum Foldable Foldable a b = (Foldable ** Num (fold a + fold b))
    routeSum Mix Foldable a b = (resolve Mix Foldable ** Sum a b)
    routeSum Foldable Mix a b = (resolve Foldable Mix ** Sum a b)
    routeSum Mix Mix a b = (resolve Mix Mix ** Sum a b)
    routeSum NFold Foldable a b = (resolve NFold Foldable ** Sum a b)
    routeSum Foldable NFold a b = (resolve Foldable NFold ** Sum a b)
    routeSum NFold Mix a b = (resolve NFold Mix ** Sum a b)
    routeSum Mix NFold a b = (resolve Mix NFold ** Sum a b)
    routeSum NFold NFold a b = (resolve NFold NFold ** Sum a b)

normalize (resolve x y) (Prod {x} {y} a b) = 
  let (x' ** a') = normalize x a
      (y' ** b') = normalize y b
  in routeProd x' y' a' b'
  where
    routeProd : (x,y : Status) -> (a : Alg x) -> (b : Alg y) -> (t : Status ** Alg t)
    routeProd Foldable Foldable a b = (Foldable ** Num (fold a * fold b))
    routeProd Mix Foldable a b = (resolve Mix Foldable ** Prod a b)
    routeProd Foldable Mix a b = (resolve Foldable Mix ** Prod a b)
    routeProd Mix Mix a b = (resolve Mix Mix ** Prod a b)
    routeProd NFold Foldable a b = (resolve NFold Foldable ** Prod a b)
    routeProd Foldable NFold a b = (resolve Foldable NFold ** Prod a b)
    routeProd NFold Mix a b = (resolve NFold Mix ** Prod a b)
    routeProd Mix NFold a b = (resolve Mix NFold ** Prod a b)
    routeProd NFold NFold a b = (resolve NFold NFold ** Prod a b)

ex1 : Alg Foldable
ex1 = Prod (Sum (Num 5) (Num 3)) (Sum (Num 2) (Num 4))

ex2 : Alg Mix
ex2 = Sum (Prod (Num 2) (Num 3)) (Var X)

ex3 : Alg Mix
ex3 = Prod (Sum (Num 5) (Num 3)) (Sum (Var X) (Num 2))

ex4 : Alg NFold
ex4 = Prod (Var X) (Var Y)

main : IO ()
main = do
  putStrLn "Example 1: Pure numbers - should fully evaluate to a single number"
  putStrLn ("Original: " ++ show ex1)
  let (s1 ** n1) = normalize Foldable ex1
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
  
  putStrLn "Example 4: All variables - should stay as NFold"
  putStrLn ("Original: " ++ show ex4)
  let (s4 ** n4) = normalize NFold ex4
  putStrLn ("Normalized: " ++ show n4 ++ " (Status: " ++ show s4 ++ ")")
  putStrLn ""
  
  pure()
