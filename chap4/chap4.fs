module Chap4.exs

open System;;

(*4.1*)
let upto n = [1..n];;
(*4.2*)
let downto1 n = [n..(-1)..1];;
(*4.3*)
let evenN n = [2..2..n];;

(*4.4*)
(**)

(*********************)
let rec filter p = function 
  | []               -> []
  | x::rest when p x -> filter p rest
  | x::rest          -> x::(filter p rest)

(*4.5*)
let odd n = n%2 = 0 ;;

let rmodd = filter odd;;
  
(*4.6*)
let even n = not (odd n)

let rmeven = filter even;;

(*4.7*)
let rec multiplicity x = function 
  | []            -> 0
  | h::t when x=t -> 1 + multiplicity x t 
  | _::t          -> multiplicity x t;;
  
(*4.8*)

let rec split = function
  | []         -> ([], [])
  | [x]        -> failwith "invalid number of list elements" 
  | x::y::rest -> let (xs, ys) = split rest in (x::xs, y::ys);;
                  
(*4.9*)
let rec zip xx yy = 
  match (xx, yy) with 
    | ([],[])        -> []
    | (x::xs, y::ys) -> (x,y)::(zip xs ys)
    | _              -> failwith "invalid number of elements"

(*4.10*)
let rec prefix xx yy =
  match (xx, yy) with
    | (x::xs, y::ys)  -> x=y && (prefix xs ys)
    | ([], _)         -> true
    | _               -> false;;
    
(*4.11*)
let rec count(xs, x) = 
  match xs with 
    | []             -> 0
    | y::ys when x=y -> 1 + count(ys, x)
    | ys             -> count(ys, x);;
    
let rec insert(xs, x) =
  match xs with
    | []                                   -> []
    | [y] when x<=y                        -> x::[y]
    | [y]                                  -> y::[x]
    | y0::(y1::_ as ys) when x>y0 && x<=y1 -> y0::x::y1::ys
    | ys                                   -> insert(ys, x)
    
let rec intersect (xs, ys) =
  match (xs, ys) with 
    | (_, [])                 -> xs
    | ([], _)                 -> ys
    | (v::vs, w::ws) when v=w -> v::intersect(vs,ws)
    | (v::vs, w::_) when v<w  -> intersect(vs, ys)
    | (_, _::ws)              -> intersect(xs, ws);;

let rec plus (xs, ys) = 
  match (xs, ys) with 
    | (_, [])                 -> xs
    | ([], _)                 -> ys
    | (v::vs, w::ws) when v=w -> v::w::plus(vs,ws)
    | (v::vs, w::_) when v<w  -> v::plus(vs, ys)
    | (_, w::ws)              -> w::plus(xs, ws);;

let rec minus (xs, ys) = 
  match (xs, ys) with
    | (_, [])                 -> xs
    | ([], _)                 -> []
    | (v::vs, w::ws) when v=w -> minus(vs,ws)
    | (v::vs, w::_) when v<w  -> v::minus(vs, ys)
    | (_, w::ws)              -> minus(xs, ws);;
    