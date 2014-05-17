module Curve

type Point = float*float
type Points = Point list

type Curve = C of Point * Points
 
let map f (C(p0, ps)) = C(f p0, List.map f ps)  
let mapTail f (C(p0, ps)) = C(p0, List.map f ps)
 
type Curve with
  static member ( + ) (c1, c2) = 
    match (c1, c2) with 
      | (C(h1, ps1), C(h2, ps2)) -> C(h1, ps1@(h2::ps2))
  static member ( * ) (k, (C((x0, y0), _) as c)) = 
    let multK (x,y) = (x0 + k*(x - x0), y0 + k*(y-y0)) 
    mapTail multK c   

