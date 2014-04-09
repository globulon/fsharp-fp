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

let real {real = r; img = _} = r: float;;
let img {real = _; img = i} = i: float;;

let makeComplex x y = { real = x; img = y };;

let (.+.) {real = r1; img = i1} {real = r2; img = i2} = makeComplex (r1 + r2) (i1 + i2);;

let (.*.) {real = r1; img = i1} {real = r2; img = i2} = makeComplex (r1*r2 - i1*i2) (r1*i2 + r2*i1);;

let (~-) {real = r; img = i}  = makeComplex -r -i;; 

let norm  {real = r; img = i} = (r*r + i*i) ;;

let (!@) z = let d = norm z
             makeComplex ((real z)/d) ((img -z)/d);;              

let (.-.) z1 z2 = z1 .+. -z2;;

let (./.) z1 z2 = z1 .*. !@z2;;


(* 3.4 *)
type StraighLine = float * float;;

let makeLine a b = (a, b): StraighLine;;

let mirrorX (x, y) = (-x, -y);;

let mirrorY (x, y) = (-x, y);;
 
let lineToString (x, y) = (string x) + "x" + (string y);;

(* 3.5 see: solve.fs *)


