module Chap4.Reg

open System;;

type ArticleCode  = string;;
type ArticleName  = string;;
type Price        = int;;

type Register     = (ArticleCode * (ArticleName * Price)) list;;

type NoPieces     = int;;
type Item         = NoPieces * ArticleCode;;
type Purchase     = Item list;;

type Info         = NoPieces * ArticleName * Price ;;
type InfoSeq      = Info list;;
type Bill         = InfoSeq * Price;;


let reg: Register = [("a1", ("cheese", 25));
                     ("a2", ("herring", 4));
                     ("a3", ("soft drink", 5))];;

let pur: Purchase = [(3, "a2"); (1, "a1")];;

let rec findArticle reg code =  
  match reg with 
    | (c, desc)::_ when c = code -> desc
    | _::rs                      -> findArticle rs code
    | []                         -> failwith "article not found";;
    

let rec makeBill reg pur = 
  match pur with
    | []              -> ([], 0)
    | (n, code)::rest -> let (name, price) = findArticle reg code
                         let (items, sum)  = makeBill reg rest
                         let itemPrice     = n*price
                         let item          = (n, name, itemPrice)
                         (item::items, sum + itemPrice);;

(* let makeBill: Register -> Purchase -> Bill;; *)