module Chap5.Exs

open System;;

(*5.1*)
let filterL p xs = 
  let f a acc = if p a then a::acc else acc
  List.foldBack f xs [];;

let filterM p m = 
  let f a b m' = if p a b 
                 then Map.add a b m'
                 else m'
  Map.foldBack f m Map.empty;;
  
(*5.2*)
let reverseL xs = List.fold (fun acc a -> a::acc) [] xs ;;

(*5.3*)
let sum p = (List.filter p) >> (List.fold (+) 0);;

(*5.4*)
let downto1 f n e = List.foldBack f [1..n] e;; 

let fac n = downto1 (*) n 1;;

let mapN g n = downto1 (fun x xs -> g(x)::xs) n [] ;;

(*5.5*)
  ///Skip on Map example
  
(*5.6*)
let dom r: Set<'a> = Set.map fst r;;
let rng r: Set<'b> = Set.map snd r;;

let apply r a = rng (Set.filter (fun (k, _) -> k = a) r);;

  /// S = R U {(y,x):(x,y) E R}
let symmetric r = 
  let yx = Set.fold (fun s (x, y) -> Set.add (y, x) s) Set.empty r
  Set.union r yx ;;
  
  /// compose 
let compose r s = 
  let img (a, b) = Set.map (fun c -> (a, c)) (apply s b)
  Set.fold (fun acc ab -> Set.union (img ab) acc) Set.empty r;;
  
  /// transitive 
let transitive r = 
  let composeN curr acc = let rr = compose curr r
                          (rr, Set.union rr acc)
  snd (Set.fold (fun (curr, acc) _ -> composeN curr acc) (r, Set.empty) r);;
  

let r = Set.ofList [("a", 1); ("b",2);("c", 3); ("c", 4); ("b", 4)];;
let s = Set.ofList [(1,2);(1,1);(2,1); (1,3)];;

(*5.8*)
let rec powset s t = 
  match s with 
    | _ when Set.isEmpty s -> set [t]
    | _                    -> let m  = Set.minElement s
                              let s' = Set.remove m s 
                              Set.union (powset s' t) (powset s' (Set.add m t));;
                              
let size s = Set.fold (fun acc _ -> acc + 1) 0 s;;

let allSubsets n k = Set.filter (fun s -> k = (size s)) (powset n Set.empty);;;                            

