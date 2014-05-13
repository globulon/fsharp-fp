module Vector

type Vector 

val ( ~-. ) : Vector -> Vector
val ( +. )  : Vector -> Vector -> Vector
val ( -. )  : Vector -> Vector -> Vector
val ( *. )  : float  -> Vector -> Vector
val ( &. )  : Vector -> Vector -> float
val norm    : Vector -> float
val make    : float * float -> Vector
val coord   : Vector -> float * float