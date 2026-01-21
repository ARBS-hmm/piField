module Algebra.Ring
import Algebra.Group

export
infixl 8 .*

interface Group a => Ring a where
  (.*) : a -> a -> a
  one : a
  multAssoc : (x, y, z : a) -> ((x .* y) .* z) = (x .* (y .* z))
  multLeftId : (x : a) -> one .* x = x
  multRightId : (x : a) -> x .* one = x
  leftDistrib : (x, y, z : a) -> x .* (y .+ z) = (x .* y) .+ (x .* z)
  rightDistrib : (x, y, z : a) -> (x .+ y) .* z = (x .* z) .+ (y .* z)
  multLeftZero : (x : a) -> Group.e .* x = Group.e
  multRightZero : (x : a) -> x .* Group.e = Group.e

zeroProdl : Ring a => (x : a) -> (e .* x) = e
zeroProdr : Ring a => (x : a) -> (x .* e) = e

negProdl : Ring a => (x : a) -> ((inv x) .* y) = (inv (x .* y))
negProdr : Ring a => (x,y : a) -> (x .* (inv y)) = (inv (x .* y))


