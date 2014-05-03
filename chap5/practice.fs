module Chap5.Practice

open System;;

type ArticleCode  = string;;
type ArticleName  = string;;
type Price        = int;;

type Register     = Map<ArticleCode, ArticleName*Price>;;

type NoPieces     = int;;
type Item         = NoPieces * ArticleCode;;
type Purchase     = Item list;;

type Info         = NoPieces * ArticleName * Price ;;
type InfoSeq      = Info list;;
type Bill         = InfoSeq * Price;;


let rec makeBill1 reg = function 
  | []                   -> ([], 0)
  | (np, code)::purchase -> 
      match Map.tryFind code reg with
        | Some (name, price) -> let itemPrice = np*price
                                let (infos, total)  = makeBill1 reg purchase
                                ((np, name, itemPrice)::infos, itemPrice+total)
        | None               -> failwith ("Unknown article " + code);;


(*practice fold left instead of fold right for  practice purpose*)                                                      
let makeBill reg pur =
  let f (infos, total) np code = 
    let (name, price) = Map.find code reg
    let itemPrice = np*price
    ((np, name, itemPrice)::infos, itemPrice+total)
  Map.fold f ([], 0) pur;;

let reg1 = Map.ofList [("a1", ("cheese", 25));
                       ("a2", ("herring", 4));
                       ("a3", ("soft drink", 5))];;

                       
let pur = [(3, "a2"); (1, "a1")];;
let pur1 = Map.ofList pur;;


