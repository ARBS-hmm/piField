module Tree.ProdLater

data St = N | A | M | Ill
data V = X | Y | Z

total
sumResolve : St -> St -> St
sumResolve N M = A
sumResolve A M = A
sumResolve M N = Ill
sumResolve M A = Ill
sumResolve N N = A
sumResolve N A = A
sumResolve _ _ = Ill

total
prodResolve : St -> St -> St
prodResolve N N = M
prodResolve N M = M
prodResolve M N = M
prodResolve _ _ = Ill

data Expr : St -> Type where
  Num : Nat -> Expr N
  Var : V -> Expr N
  Sum : {x,y : St} -> Expr x -> Expr y -> Expr (sumResolve x y)
  Prod : {x,y : St} -> Expr x -> Expr y -> Expr (prodResolve x y)

Cannonical : Type
Cannonical = Expr A

Show V where
  show X = "x"
  show Y = "y"
  show Z = "z"
  
Show (Expr s) where
  show (Num n) = show n
  show (Var v) = show v
  show (Sum l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Prod l r) = "(" ++ show l ++ " * " ++ show r ++ ")"

coeffHelper : (s : St) -> Expr s -> List Nat
coeffHelper N (Num n) = [n]
coeffHelper N (Var v) = [0]
coeffHelper (sumResolve x y) (Sum {x} {y} l r) = 
  coeffHelper x l ++ coeffHelper y r
coeffHelper (prodResolve x y) (Prod {x} {y} l r) = 
  coeffHelper x l ++ coeffHelper y r
coeffHelper _ _ = []

coefficients : Expr A -> List Nat
coefficients = coeffHelper A

ok1 : Expr A  -- a + b + c*d
ok1 = Sum (Sum (Var X) (Var Y)) (Prod (Var Z) (Num 5))

ok2 : Expr Ill  -- a + b*c + d (violates ordering!)
ok2 = Sum (Sum (Var X) (Prod (Var Y) (Var Z))) (Var X)

main : IO ()
main = do
  pure ()
