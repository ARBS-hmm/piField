module Lang.ProdLater

{--
num → N
var → N

add(N, M) → A   (allowed: num + mul)
add(A, M) → A   (allowed: (num+mul) + mul)
add(M, N) → BAD (disallowed: mul + num)
add(M, A) → BAD (disallowed: mul + add)
add(N, N) → A   (allowed: num + num)
add(_, _) → BAD if any child is BAD

mul(N, N) → M   (allowed: num * num)
mul(N, M) → M   (allowed: num * mul)
mul(M, N) → M   (allowed: mul * num)
mul(_, _) → BAD otherwise
--}

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
  Sum : Expr x -> Expr y -> Expr (sumResolve x y)
  Prod : Expr x -> Expr y -> Expr (prodResolve x y)

Cannonical : Type
Cannonical = Expr A

e : Cannonical
e =
  Sum
    (Num 1)
    (Sum
      (Num 4)
      (Prod (Num 2) (Num 3)))

eIll : Expr Ill
eIll =
  Sum
    (Prod (Num 2) (Num 3))
    (Num 4)
