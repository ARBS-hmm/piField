module Algebra.Group
%default total

export
infixl 6 .+

public export
interface Group a where
  public export
  (.+) : a -> a -> a
  inv : a -> a
  e : a
  assoc : (x, y, z : a) -> ((x .+ y) .+ z) = (x .+ (y .+ z))
  leftId : (x : a) -> e .+ x = x
  rightId : (x : a) -> x .+ e = x
  leftInv : (x : a) -> (inv x) .+ x = e
  rightInv : (x : a) -> x .+ (inv x) = e

leftIdUnique : Group a => {z : a} -> ((x : a) -> z .+ x = x) -> z = Group.e
leftIdUnique {z} eleftid = trans (sym (rightId z)) (eleftid Group.e)

rightIdUnique : Group a => {z : a} -> ((x : a) -> x .+ z = x) -> z = Group.e
rightIdUnique {z} erightid = trans (sym (leftId z)) (erightid Group.e)

leftInvUnique : Group a => (x, y : a) -> (x .+ y = Group.e) -> (x = inv y)
leftInvUnique x y given = 
  trans (sym (rightId x))
        (trans (cong (x .+) (sym (rightInv y)))
        (trans (sym (assoc x y (inv y)))
        (trans (cong (.+ inv y) given)
               (leftId (inv y)))))

rightInvUnique : Group a => (x, y : a) -> (x .+ y = Group.e) -> y = inv x
rightInvUnique x y given =
  trans (sym (leftId y))
        (trans (cong (.+ y) (sym (leftInv x)))
        (trans (assoc (inv x) x y)
        (trans (cong (inv x .+) given)
               (rightId (inv x)))))
