module Chap3.Equation

open System;;

exception SolveError;;

type Equation = float * float * float ;;

type Solution = float * float ;;

type Solve = Equation -> Solution ;;

let delta = fun (a,b,c) -> b*b - 4.0*a*c ;;

(* uses raise *)
let solve: Solve =  function 
  | (a, _, _) when a = 0.0             -> raise SolveError 
  | (a, b, c) when delta(a,b,c) < 0.0  -> raise SolveError
  | (a, b, c)                          -> ((-b - sqrt(delta(a,b,c)))/(2.0*a),
                                           (-b + sqrt(delta(a,b,c)))/(2.0*a));;

(* uses failwith *)                                           
let solve2: Solve =  function 
  | (a, _, _) when a = 0.0             -> failwith "null a"
  | (a, b, c) when delta(a,b,c) < 0.0  -> failwith  "discriminant is negative"
  | (a, b, c)                          -> ((-b - sqrt(delta(a,b,c)))/(2.0*a),
                                           (-b + sqrt(delta(a,b,c)))/(2.0*a));;


(* uses locally declared identifier*)
let solve3: Solve =  function 
  | (a, _, _) when a = 0.0             -> failwith "null a"
  | (a, b, c) -> let d = delta(a,b,c)
                 if d < 0.0 
                 then failwith  "discriminant is negative"
                 else ((-b - sqrt(d))/(2.0*a), (-b + sqrt(d))/(2.0*a));;
                 