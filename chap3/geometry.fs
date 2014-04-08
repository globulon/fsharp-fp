module Chap3.Geom

open System;;

let square r = r*r: float;;

type Shape = | Circle of float
             | Square of float
             | Triangle of float*float*float;;
             
let area = function 
  | Circle r        -> System.Math.PI * (square r)
  | Square r        -> (square r)
  | Triangle(a,b,c) -> let s = (a + b + c) /2.0
                       sqrt(s*(s - a)*(s - b)*(s - c));; 