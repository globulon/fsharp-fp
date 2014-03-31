module FP.Chap1

open System;;

let plusFour n = n + 4;;

let sqr x: float = x * x;;

let norm x y = System.Math.Sqrt ( sqr x + sqr y);;

let dec n = n-1;;

let rec sumInt = function 
    | 1 -> 1
    | n -> n + sumInt (dec n);;
    
let rec fibo = function
    | 0 -> 1
    | 1 -> 1
    | n -> (fibo (n-1)) + (fibo (n-2));;
    
let rec sum = function
    | (m, 0) -> m
    | (m, n) -> m + n + sum(m, n-1);; 