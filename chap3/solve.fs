module Chap3.Solve

open System;;
(* Solves 3.5 *)
exception SolveError;;

type Equation = float * float * float ;;

type Solution = | NoSolution
                | OneSolution of float
                | TwoSolutions of float*float ;;

type Solve = Equation -> Solution ;;

let delta = fun (a,b,c) -> b*b - 4.0*a*c ;;

let solve: Solve =  function 
  | (a, _, _) when a = 0.0             -> NoSolution
  | (a, b, c) when delta(a,b,c) < 0.0  -> NoSolution
  | (a, b, c) when delta(a,b,c) = 0.0  -> OneSolution(-b / 2.0*a)
  | (a, b, c)                          -> TwoSolutions((-b - sqrt(delta(a,b,c)))/(2.0*a),
                                                       (-b + sqrt(delta(a,b,c)))/(2.0*a));;
