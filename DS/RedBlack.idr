module DS.RedBlack

data Color = Red | Black

data RBT : Nat -> Color -> Nat -> Type -> Type where
  Nil : RBT 0 Black 0 a
  RedNode : (val : a) -> 
            RBT n Black bh a ->
            RBT m Black bh a ->
            RBT (S(n+m)) Red bh a
  BlackNode : (val : a) ->
              RBT n cl bh a ->
              RBT m cr bh a -> 
              RBT (S(m+n)) Black (S bh) a
