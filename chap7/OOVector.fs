module Vector

type Vector = 
  | V of float * float
  static member ( ~- ) (V(x, y))           = V(-x, -y)
  static member ( + ) (V(x, y), V(x', y')) = V(x+x', y+y')
  static member ( - ) (V(x, y), V(x', y')) = V(x-x', y-y')
  static member ( * ) (k, V(x, y))         = V(k*x, k*y)
  static member ( * ) (V(x, y), V(x', y')) = x*x' + y*y'
  
let norm  (V(x, y))              = sqrt(x*x + y*y)
let make (x, y)                  = V(x,y)
let coord (V(x,y))               = (x,y)