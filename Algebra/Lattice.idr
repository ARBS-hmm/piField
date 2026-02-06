module Algebra.Lattice
%default total

interface Lattice a where
  meet : a -> a -> a
  join : a -> a -> a

  meetCommute : (x,y: a) -> (meet x y) = (meet y x)
  joinCommute : (x,y: a) -> (join x y) = (join y x)

  assocMeet : (x,y,z:a) -> (meet (meet x y) z) = (meet x (meet y z))
  assocJoin : (x,y,z:a) -> (join (join x y) z) = (join x (join y z))

  meetIdempotent : (x : a) -> meet x x = x
  joinIdempotent : (x : a) -> join x x = x

  absorbMeetJoin : (x, y : a) -> meet x (join x y) = x
  absorbJoinMeet : (x, y : a) -> join x (meet x y) = x

interface Lattice a => BoundedLattice a where
  top : a
  bottom : a
  topMax : (x:a) -> meet x top = x
  bottomMin : (x:a) -> join x bottom = x

export
Lattice Bool where
  meet False _ = False
  meet True y = y
  
  join True _ = True
  join False y = y
  
  meetCommute False False = Refl
  meetCommute False True = Refl
  meetCommute True False = Refl
  meetCommute True True = Refl
  
  joinCommute False False = Refl
  joinCommute False True = Refl
  joinCommute True False = Refl
  joinCommute True True = Refl
  
  assocMeet False y z = Refl
  assocMeet True y z = Refl
  
  assocJoin False y z = Refl
  assocJoin True y z = Refl
  
  meetIdempotent False = Refl
  meetIdempotent True = Refl
  
  joinIdempotent False = Refl
  joinIdempotent True = Refl
  
  absorbMeetJoin False y = Refl
  absorbMeetJoin True y = Refl
  
  absorbJoinMeet False y = Refl
  absorbJoinMeet True y = Refl

BoundedLattice Bool where
  top = True
  bottom = False

  topMax False = Refl
  topMax True = Refl

  bottomMin False = Refl
  bottomMin True = Refl
