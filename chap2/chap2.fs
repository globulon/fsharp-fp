module FP.Chap2

open System;;


(* 2.1 *)
let (/%/) x y = x%y = 0 ;;

let divByTwo x = x /%/ 2 ;; 

let divByThree x = x /%/ 3;;

let divByFive x = x /%/ 5;;

let weirdNumber x = (divByTwo x) || ((divByThree x) && (not (divByFive x))) ;;


(* 2.2 *)
let rec pow = function 
  | (s, 0) -> s : string (*forces inference*)
  | (s, n) -> s + (pow(s, n-1));; 
  
(* 2.3 *)
let isIthChar str i chr = (i < String.length str) && (str.[i] = chr);;

(* 2.4 *)
let rec occFromIth = function
  | (str, i, _) when (i >= String.length str) -> 0
  | (str, i, chr) when str.[i] = chr          -> 1 + occFromIth(str, (i+1), chr)
  | (str, i, chr)                             -> occFromIth(str, (i+1), chr);;
  
(* 2.5 *)
let rec occInString(str, ch) = occFromIth(str, 0, ch);;

(* 2.6 *)
let notDivisible(d,n) = not (n%d = 0);;

(* 2.7 *)
let rec test = function
  | (a, b, _) when a > b  -> true
  | (a, b, c)             -> notDivisible(a, c) && test(a+1, b, c);;
  
let prime = function
  | 2 -> true
  | n -> test(2, n-1, n);;
  
let rec nextPrime n = if (prime(n+1)) then n+1 else nextPrime(n+1);;

(* 2.8 *)

let rec pascal = function
  | (n, 0) -> 1
  | (n, m) when n = m -> 1
  | (n,k) -> pascal(n-1, k-1) + pascal(n-1, k);;
  
(* 2.11 *)
let factor n = (1.0 + (float n) / 100.0);;

let VAT n x = x * factor n;;

let unVAT n x = x / factor n;;

(* 2.12 *)

(* 2.13 *)
let curry f = fun x y -> f(x,y);;

let uncurry f = fun (x,y) -> f x y;;  