module Chap3.Exs

open System;;

(* commons *)
let square x = x * x;;

(* ex 3.1 *)
type TimeOfDay = int*int*string;;

let hour: TimeOfDay -> int    = fun (h, _, _) -> h;;
let mins: TimeOfDay -> int    = fun (h, m, _) -> m;;
let part: TimeOfDay -> string = fun (_,_,p) -> p;;

type Compare = TimeOfDay*TimeOfDay -> bool;;

let isDayPart: string -> bool = fun p -> p = "AM" || p = "PM"

let (<<) (h1:int, m1:int, p1) (h2, m2, p2) =  
  match (p1, p2) with
    | ("AM", "PM")                        -> true
    | ("PM", "AM")                        -> false
    | (p, q)  when p = q && (isDayPart p) -> h1 <= h2 && m1 <= m2
    | _                                   -> failwith "invalid day part";;
  
 (* 3.3 *)
 type Complex = {real: float; img: float};;
 
 let real {real = r; img = _} = r;;
 let img {real = _; img = i} = i;;
  
 let makeComplex x y = { real = x; img = y };;
  
 let (.+.) {real = r1; img = i1} {real = r2; img = i2} = {real = r1 + r2; img = i1 + i2};;
 
 let (.*.) {real = r1; img = i1} {real = r2; img = i2} = {real = r1*r2 - i1*i2; img = r1*i2 + r2*i1 };;
 
 
                                