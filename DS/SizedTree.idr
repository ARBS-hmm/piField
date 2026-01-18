module DS.SizedTree

data Tree : Nat -> Type -> Type where
  Nil : Tree 0 a
  Leaf : a -> Tree 1 a
  Node : a -> Tree n a -> Tree m a ->
         Tree (S(m+n)) a

test2 : Tree 7 Nat
test2 = Node 1 
          (Node 2 
            (Node 3 (Leaf 4) (Leaf 5))
            (Leaf 6))
          (Leaf 7)
