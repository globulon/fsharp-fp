module Curve

type Point = float*float

[<Sealed>]
type Curve =
  static member ( + ) : Curve * Curve -> Curve
  static member ( * ) : float -> Curve -> Curve
