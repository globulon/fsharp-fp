module Chap3.Rat

open System ;;

type Qnum = int*int;;

let numer: Qnum -> int = fst

let denum: Qnum -> int = snd

type EndoQnum = Qnum -> Qnum

let sign: Qnum -> int = fun (n, d) -> if n*d < 0 then -1 else 1;;

let rec gcd = function 
  | (0, n) -> n
  | (m, n) -> gcd(n%m, m);;
  
let reduce: EndoQnum   = 
  fun q -> let g = gcd q in ((sign q)*(abs (numer q)) / g, abs (denum q) / g);;

let makeQ = function
  | (_, 0) -> failwith "rational division by zero"
  | q      -> reduce q;; 
 
 let toString q = (string (numer q)) + "/" + (string (denum q));;