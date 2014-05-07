module DQueue

type DQueue<'a> = { front: 'a list; rear: 'a list } 

let empty                           = 
  { front = List.empty<'a>; rear = List.empty<'a> };;
  
let isEmpty q                       = 
  (List.isEmpty q.front) && (List.isEmpty q.rear);;
  
let put a { front = xs; rear = ys } = 
  { front = xs; rear = a::ys };; 
  
let rec get                         = function   
  | { front = x::xs; rear = ys } -> (x, { front = xs; rear = ys })
  | { front = []; rear = ys }    -> get { front = List.rev ys; rear = [] };;
