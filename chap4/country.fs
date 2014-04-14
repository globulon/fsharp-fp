module Chap4.Country

open System;;

type Country  = string;;
type Map      = (Country * Country) list;;
type Color    = Country list;;
type Colors   = Color list;;

let isMember a = function 
  | x::xs when a=x -> true
  | _              -> false;;
  
let areNb c1 c2 xs = 
  (isMember (c1, c2) xs) || (isMember (c2, c1) xs);;

let rec canBeExtBy m color country =  
  match color with 
    | c::cs  -> not (areNb m c country) && canBeExtBy m cs country
    | _      -> true;;