module Chap4.Pract

open System;;

let rec suml = function
  | []    -> 0
  | x::xs -> x + suml xs;;
  
let rec altsum = function
  | []          -> 0
  | [x]         -> x
  | x0::x1::xs  -> x0 - x1 + altsum xs ;;
  
  
let rec succPair = function
  | x0::x1::(_ as xs)  -> (x0,x1):: succPair xs
  | _                  -> [];;
  
let rec sumProd = function
  | [] -> (0, 1)
  | x::xs -> let (sum, prod) = sumProd xs
             (x + sum, x * prod);;

let rec unzip = function 
  | []        -> ([], [])
  | (x,y)::ps -> let (xs, ys) = unzip ps
                 (x::xs, y::ys);;

let rec mix = function
  | (x::xs, y::ys) -> x::y::mix(xs, ys)
  | ([], []) -> []
  | _         -> failwith "list are not the same size"
  
let rec memberOf x = function
  | []      -> false
  | (y::ys) -> x=y || memberOf x ys;;