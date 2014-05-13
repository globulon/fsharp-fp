module Vector

type Vector = V of float * float

let ( ~-. ) (V(x, y))            = V(-x, -y)
let ( +. ) (V(x, y)) (V(x', y')) = V(x+x', y+y')
let ( -. ) (V(x, y)) (V(x', y')) = V(x-x', y-y')
let ( *. ) k (V(x, y))           = V(k*x, k*y)
let ( &. ) (V(x, y)) (V(x', y')) = x*x' + y*y'
let norm  (V(x, y))              = sqrt(x*x + y*y)
let make (x, y)                  = V(x,y)
let coord (V(x,y))               = (x,y)