(* abstract syntax tree *)

type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Var of ident
  | Let of ident * expr * expr

(* CSE *)

let alpha_equiv (e1 : expr) (e2 : expr) : bool  =
  let rec doit (e1 : expr) (e2 : expr) (l1 : ident list) (l2 : ident list) : bool= 
    match e1, e2 with
    | Int a, Int b -> a = b
    | Bool a, Bool b -> a = b
    | Var a, Var b -> if List.find_index (fun x -> x = a) l1 = None then a=b else (List.find_index (fun x -> x = a) l1) = (List.find_index (fun x -> x = b) l2)
    | Binop (op1 , p1 , p2) , Binop (op2 , q1 , q2)  -> op1 = op2 && doit p1 q1 l1 l2 && doit p2 q2 l1 l2
    | If (p1,p2,p3) , If (q1,q2,q3) -> doit p1 q1 l1 l2 && doit p2 q2 l1 l2 && doit p3 q3 l1 l2
    | Let (id1,p1,p2) , Let (id2,q1,q2) -> let new_l1 = id1::l1 and new_l2 = id2::l2 in 
                                            doit p1 q1 l1 l2 && doit p2 q2 new_l1 new_l2
    | _ , _ -> false
  in doit e1 e2 [] []


let rec wystapil (id : ident) (e : expr) : bool =
  (*czy identyfikator wystapil w wyrazeniu*)
  match e with
  | Int _ -> false
  | Bool _ -> false
  | Var a -> a = id
  | Binop (_, e1, e2) -> wystapil id e1 || wystapil id e2
  | If (e1, e2, e3 ) -> wystapil id e1 || wystapil id e2 || wystapil id e3
  | Let (id1,e1,e2) -> if id1 = id then true else wystapil id e1 || wystapil id e2
    

let rec find (target : expr) (tree : expr) : bool = 
  (*czy poddrzewo target wystapilo w drzewie tree (w sensie alfa-rownowaznosci)*)
  alpha_equiv target tree || 
  (* dalsza czesc ma sie jedynie zaglebiac*)
  match tree with 
  | Int _ -> false
  | Bool _ -> false
  | Var _ -> false
  | Binop (_, e1, e2) -> find target e1 || find target e2
  | If (e1,e2,e3) -> find target e1 || find target e2 || find target e3
  | Let (id1,e1,e2) -> find target e1 || if wystapil id1 target then false else find target e2
     

let rec find_and_replace (target : expr) (tree : expr) (id : ident): expr  = 
  (*zamienia wszystkie wystapienia poddrzewa target w drzewie tree identyfikatorem id*)
  if alpha_equiv target tree then Var id else 
  (* dalsza czesc ma sie jedynie zaglebiac*)
  match tree with 
  | Int _-> tree
  | Bool _ -> tree
  | Var _ -> tree
  | Binop (op, e1, e2) -> Binop(op,find_and_replace target e1 id, find_and_replace target e2 id)
  | If (e1,e2,e3) -> If(find_and_replace target e1 id , find_and_replace target e2 id , find_and_replace target e3 id)
  | Let (id1,e1,e2) -> Let(id1,find_and_replace target e1 id,if wystapil id1 target = false then find_and_replace target e2 id else e2)
    


  
let rec find_target (target_tree : expr) (search_tree : expr) : expr option = 
  (*szuka poddrzewa z target_tree ktore wystepuje w search_tree*)
  let finder (e1:expr) (f : expr option): expr option = 
    (*finduje*)
    (*znajduje cel ktory mozna wyjmowac*)
    match find_target e1 search_tree with
      |Some e' -> Some e'
      |None -> f
  in
  match target_tree with 
  |Int _ -> None
  |Bool _ -> None
  |Var _ -> None
  |_ ->
  if find target_tree search_tree then Some target_tree else
    (* dalsza czesc ma sie jedynie zaglebiac*)
    match target_tree with 
    | Int _-> None
    | Bool _ -> None
    | Var _ -> None
    | Binop(_,e1,e2) -> (finder e1 (finder e2 None))
    | If(e1,e2,e3) -> (finder e1 (finder e2 (finder e3 None)))
    | Let(id1,e1,e2) -> (finder e1 (if wystapil id1 e2 then None else (finder e2 None)))



(*
1. majac korzen, generuj targety z lewego i szukaj w prawym, potem na odwrot
2. jezeli znajdziesz target w prawym, to wyjmij do leta
3. zwracaj wyjecie z leta jako Some lub none
*)
let cse (e : expr) : expr option = 
  (*iDelti to nowoczesny na generowanie nazw w zaleznosci od tego w ktore poddrzewo sie wybieramy
     majac tym samym pewnosc ze nigdy nie powtorzymy jej dwukrotnie*)
  let rec help_cse (e : expr) (iDelti : ident) : expr option = 
    let extractor (e1:expr) (e2:expr) (f : expr option) : expr option = 
      (*extractuje*)
      (*jego zadanie to wyjecie wspolnego podpoddrzewa dwoch poddrzew*)
      (match find_target e1 e2  with
      |Some e' -> Some (Let(iDelti,e',find_and_replace e' e iDelti ))
      |None -> f)
    in
    match e with
    | Int _ -> None
    | Bool _ -> None
    | Var _ -> None
    | Binop (op,e1,e2) -> (extractor e1 e2 (extractor e2 e1
      (*
      Match z help_cse ma za zadanie wywolac sie rekurencyjnie na poddrzewach oraz 
      extractowac z nich wyrazenia rekurencyjnie   
      *)
        (match help_cse e1 (iDelti^"L") with
        |Some e' -> Some (Binop (op,e',e2))
        |None -> (match help_cse e2 (iDelti^"R") with
                  |Some e' -> Some (Binop (op,e1,e'))
                  |None -> None))
      ))
    
    | If (e1, e2, e3) -> (extractor e1 e2 (extractor e1 e3 (extractor e2 e1 (extractor e2 e3 (extractor e3 e1 (extractor e3 e2   
        (match help_cse e1 (iDelti^"L") with
          |Some e' -> Some (If (e',e2,e3))
          |None -> (match help_cse e2 (iDelti^"M") with
                    |Some e' -> Some (If (e1,e',e3))
                    |None -> (match help_cse e3 (iDelti^"R") with
                              |Some e' -> Some (If (e1,e2,e'))
                              |None -> None)))
      ))))))
      
    | Let (id1, e1, e2) -> 
      if wystapil id1 e2 
        then 
          (match help_cse e1 (iDelti^"L") with
            |Some e' -> Some (Let (id1, e', e2))
            |None -> (match help_cse e2 (iDelti^"R") with
                      |Some e' -> Some (Let (id1, e1, e'))
                      |None -> None))
        else 
          (extractor e1 e2 (extractor e2 e1 
          (match help_cse e1 (iDelti^"L") with
            |Some e' -> Some (Let (id1, e', e2))
            |None -> (match help_cse e2 (iDelti^"R") with
                      |Some e' -> Some (Let (id1, e1, e'))
                      |None -> None))

    ))
  in help_cse e "#M"