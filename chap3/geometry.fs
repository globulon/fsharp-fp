module Chap3.Geom

open System;;

(* 3.6 *)

let square r = r*r: float;;

type Shape = | Circle of float
             | Square of float
             | Triangle of float*float*float;;
             
let isShape: Shape -> bool = function
  | Circle r when r > 0.0 -> true
  | Square r when r > 0.0 -> true
  | Triangle(a,b,c) 
        when a > 0.0 && 
             b > 0.0 && 
             c > 0.0 && 
             a < b + c && 
             c < a + b && 
             b < a + c    -> true
  | _                     -> false;;
  
let area = function 
  | Circle r  when isShape (Circle r)               -> System.Math.PI * (square r)
  | Square r  when isShape (Square r)               -> (square r)
  | Triangle(a,b,c) when isShape (Triangle(a,b,c))  -> let s = (a + b + c) /2.0
                                                       sqrt(s*(s - a)*(s - b)*(s - c))
  | _                                               -> failwith "Invalid shape";;