module Curve

type Point = float*float

[<Sealed>]
type Curve =
  static member ( + ) : Curve * Curve -> Curve
  static member ( * ) : Curve * float -> Curve
  static member ( |^) : Curve * float -> Curve
  static member ( |^) : Curve * int   -> Curve
  static member (-->) : Curve * Point -> Curve
  static member ( ><) : Curve * float -> Curve
  
val point      : float * float -> Curve
val verticRefl : float -> Curve -> Curve
val boundingBox: Curve -> Point * Point
val width      : Curve -> float 
val height     : Curve -> float
val toList     : Curve -> Point list
