module Curve

type Point = float*float
type Points = Point list

type Curve = C of Point * Points
 
let map f (C(p0, ps)) = C(f p0, List.map f ps)  
let mapTail f (C(p0, ps)) = C(p0, List.map f ps)
 
type Curve with
  static member ( + ) (c1, c2) = 
    match (c1, c2) with (C(h1, ps1), C(h2, ps2)) -> C(h1, ps1@(h2::ps2))
  static member ( * ) ((C((x0, y0), _) as c), k) = 
    let multK (x,y) = (x0 + k*(x - x0), y0 + k*(y-y0)) 
    mapTail multK c  
  static member ( |^) ((C((x0, y0), _) as c), theta: float) =
    let cs = cos (theta * System.Math.PI / 180.0)
    let sn = sin (theta * System.Math.PI / 180.0)
    let rotate (x,y) = 
      let dx = x - x0
      let dy = y - y0
      (x0 + cs*dx - sn*dy, y0 + sn*dx + cs*dy)
    mapTail rotate c
  static member ( |^) (c: Curve, theta: int) = c |^ (float theta) 
  static member (-->) ((C((x0, y0), _) as c), (x1,y1)) = 
    map (fun (x,y) -> (x - x0 + x1, (y - y0 + y1))) c
  static member ( ><) (c, a) =  map (fun (x,y) -> (2.0*a - x, y)) c 

let point       (x, y)            = C((x,y), [])
let verticRefl  k                 = fun c -> map (fun (x,y) -> (x, 2.0*k - y)) c
let boundingBox (C(p0, ps))       = 
  let minmax ((minX, minY), (maxX, maxY)) (x,y) = ((min minX x, min minY y), (max maxX x, max maxY y))
  List.fold minmax (p0, p0) ps 
let width      c                  = match (boundingBox c) with
                                      | ((minX, _), (maxX, _)) -> maxX - minX
let height     c                  = match (boundingBox c) with
                                      | ((_, minY), (_, maxY)) -> maxY - minY
let toList     (C(p0, ps))        = p0::ps
