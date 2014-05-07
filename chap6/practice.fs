module Chap6.Practice

open System;;

(* Chinese Cubes *)
type Colour = Red | Blue | Green | Yellow | Purple;;
type Cbox = Nothing | Cube of float * Colour * Cbox;;

let cb1 = Cube(0.5, Red, Nothing);;
let cb2 = Cube(1.0, Green, cb1);;
let cb3 = Cube(2.0, Yellow, cb2);;

let rec count = function
  | Nothing -> 0
  | Cube(_,_, cb) -> 1 + count cb;;
  
let rec insert = function
  | (s, _, _) when s <= 0.0                    -> failwith "Invalid box size"
  | (s, c, Nothing)                            -> Cube(s, c, Nothing)
  | (s, c, Cube(s', _, _)) when s=s'           -> failwith "Found cube with same size"
  | (s, c, (Cube(s', _, _) as cb)) when s > s' -> Cube(s, c, cb)
  | (s, c, Cube(s', c', cb')) when s < s' -> Cube(s', c', insert (s, c, cb'));;
  
(*Differentiation*)
type Fexpr = | Const of float
             | Var of String
             | Add of Fexpr * Fexpr
             | Sub of Fexpr * Fexpr
             | Mul of Fexpr * Fexpr
             | Div of Fexpr * Fexpr
             | Sin of Fexpr
             | Cos of Fexpr
             | Log of Fexpr
             | Exp of Fexpr;;
             
let rec D = function 
  | Const _   -> Const 0.0
  | Var _     -> Const 1.0
  | Add(l, r) -> Add(D l, D r)
  | Sub(l, r) -> Sub(D l, D r)
  | Mul(l, r) -> Add(Mul(D l, r), Mul(l, D r))
  | Div(l, r) -> Div(Sub(Mul(D l, r), Mul(l, D r)), Mul(r, r))
  | Sin f     -> Mul(Cos f, D f)
  | Cos f     -> Mul(Const -1.0, Mul(Cos f, D f))
  | Log f     -> Div(D f, f)
  | (Exp f)   -> Mul(D f, Exp f);;
  
(*Binary trees*)
///type BinTree<'a,'b> = | Leaf of 'a
///                      | Node of BinTree<'a,'b> * 'b * BinTree<'a,'b>;;
          
///let t1 = Node(Node(Leaf 1, "cd" , Leaf 2), "ab", Leaf 3);;

///let rec depth = function
///  | Leaf _         -> 0
///  | Node (l, _, r) -> 1 + depth l + depth  r;;
                      
///let rec preOrder  = function 
///  | Leaf _        -> []
///  | Node(l, v, r) -> v::preOrder l @ preOrder r;; 
  
///let rec inOrder  = function 
///  | Leaf _        -> []
///  | Node(l, v, r) -> (inOrder l) @ [v] @ (inOrder r);; 
  
///let rec postOrder  = function 
///  | Leaf _        -> []
///  | Node(l, v, r) -> (postOrder l) @  (postOrder r) @ [v];; 

///let preFold f e t = List.fold f e (preOrder t);;
//let preFoldBack f e t = List.foldBack f (preOrder t) e;;

///let rec postFoldBack f t e =
///  match t with
///    | Leaf _        -> e
///    | Node(l, v, r) -> let ex = f v e
///                       let next = postFoldBack f r ex
///                       postFoldBack f l next;;

(*Comparable trees*)
type BinTree<'a when 'a : comparison> = 
  | Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>;;;
  
let makeEmptyNode x = Node(Leaf, x, Leaf)  
  
let rec add x t = 
  match t with
    | Leaf                        -> makeEmptyNode x
    | (Node(l, x', r)) when x < x'-> Node(add x l, x', r)
    | (Node(l, x', r)) when x > x'-> Node(l, x', add x r)
    | n                           -> n;;;
    
let rec contains x t = 
  match t with 
    | Leaf                       -> false
    | Node(_, x', r) when x > x' -> contains x r
    | Node(l, x', _) when x < x' -> contains x l
    | _                          -> true;;
