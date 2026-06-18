module Tree.Fold
%default total

public export
data Status = Number | Variable | Mix

public export
Show Status where
  show Number = "Number"
  show Variable = "Variable"
  show Mix = "Mix"

public export
data V = X | Y | Z

public export
Show V where
  show X = "x"
  show Y = "y"
  show Z = "z"

public export
resolve : Status -> Status -> Status
resolve Number Number = Number
resolve Number Variable = Mix
resolve Variable Number = Mix
resolve Variable Variable = Variable
resolve Mix _ = Mix
resolve _ Mix = Mix

public export
data Alg : Status -> Type where
  Num  : Nat -> Alg Number
  Var  : V -> Alg Variable
  Sum  : {x,y : Status} -> Alg x -> Alg y -> Alg (resolve x y)
  Prod : {x,y : Status} -> Alg x -> Alg y -> Alg (resolve x y)

public export
Show (Alg s) where
  show (Num n) = show n
  show (Var v) = show v
  show (Sum {x} {y} a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Prod {x} {y} a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

public export
data Op = SumOp | ProdOp

public export
getCons : (x,y : Status) -> Op -> (Alg x -> Alg y -> Alg (resolve x y))
getCons x y SumOp = \a, b => Sum {x=x} {y=y} a b
getCons x y ProdOp = \a, b => Prod {x=x} {y=y} a b

public export
fold : Alg Number -> Nat
fold (Num n) = n
fold (Sum {x=Number} {y=Number} a b) = fold a + fold b
fold (Prod {x=Number} {y=Number} a b) = (fold a)*(fold b)
fold _ = assert_total 0 --never hit

public export
route : (x,y : Status) -> (a : Alg x) -> (b : Alg y) -> 
        (Nat -> Nat -> Nat) -> (cons : Op) ->
        (t : Status ** Alg t)

route Number Variable (Num 0) b op SumOp = (Variable ** b) 
route Variable Number a (Num 0) op SumOp = (Variable ** a) 
route Number Variable (Num 0) b op ProdOp = (Number ** (Num 0)) 
route Variable Number a (Num 0) op ProdOp = (Number ** (Num 0)) 

route Number Number a b op cons = (Number ** Num (op (fold a) (fold b)))
route x y a b op cons = (resolve x y ** (getCons x y cons) a b)

public export
normalize : (s : Status) -> Alg s -> (t : Status ** Alg t)
normalize Number (Num n) = (Number ** (Num n))
normalize Variable (Var v) = (Variable ** Var v)
normalize (resolve x y) (Sum {x} {y} a b) = 
  let (x' ** a') = normalize x a
      (y' ** b') = normalize y b
  in route x' y' a' b' (+) SumOp
normalize (resolve x y) (Prod {x} {y} a b) = 
  let (x' ** a') = normalize x a
      (y' ** b') = normalize y b
  in route x' y' a' b' (*) ProdOp


data Expr : Type where
  NumE : Nat -> Expr
  VarE : V -> Expr
  SumE : Expr -> Expr -> Expr
  ProdE : Expr -> Expr -> Expr

public export
Show Expr where
  show (NumE n) = show n
  show (VarE v) = show v
  show (SumE a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (ProdE a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

embed : Expr -> (s** Alg s)
embed (NumE n) = (Number ** (Num n))
embed (VarE x) = (Variable ** (Var x))
embed (SumE a b) = 
  let (sa ** a') = embed a
      (sb ** b') = embed b
  in ((resolve sa sb) ** Sum a' b')
embed (ProdE a b) = 
  let (sa ** a') = embed a
      (sb ** b') = embed b
  in ((resolve sa sb) ** Prod a' b')

y = embed (SumE (VarE X) (ProdE (VarE Y) (NumE 3)))
