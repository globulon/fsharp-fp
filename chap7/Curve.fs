module Curve

type Point = float*float
type Points = Point list

type Curve = C of Point * Points
  
type Curve with
  static member ( + ) (c1, c2) = 
    match (c1, c2) with (C(h1, ps1), C(h2, ps2)) -> C(h1, ps1@(h2::ps2))

