open Ast

let parse (s : string) : expr =
  Parser.prog Lexer.read (Lexing.from_string s)

type value =
  | VInt of int
  | VBool of bool

let eval_op (op : bop) (v1 : value) (v2 : value) : value =
  match op, v1, v2 with
  | Add,  VInt i1, VInt i2 -> VInt (i1 + i2)
  | Sub,  VInt i1, VInt i2 -> VInt (i1 - i2)
  | Mult, VInt i1, VInt i2 -> VInt (i1 * i2)
  | Div,  VInt i1, VInt i2 -> VInt (i1 / i2)
  | Eq,   VInt i1, VInt i2 -> VBool (i1 = i2)
  | Lt,   VInt i1, VInt i2 -> VBool (i1 < i2)
  | Gt,   VInt i1, VInt i2 -> VBool (i1 > i2)
  | Leq,  VInt i1, VInt i2 -> VBool (i1 <= i2)
  | Geq,  VInt i1, VInt i2 -> VBool (i1 >= i2)
  | Neq,  VInt i1, VInt i2 -> VBool (i1 <> i2)
  | _ -> failwith "type error"

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
  
            
  
     (*let rec go_down (target : expr) (rest : expr) (id : ident): expr = 
    if find target rest then (*znalazl i wyjmie*) Let (id,target,find_and_replace target rest id) else 
    match rest with 
    | Int _ -> rest
    | Bool _ -> rest
    | Var _ -> rest
    | Binop (op,e1,e2) -> Binop (op,go_down target e1 id,go_down target e2 id)
    | If (e1, e2, e3) -> If(go_down target e1 id, go_down target e2 id, go_down target e3 id)
    | Let (id1, e1, e2) -> Let(id1, go_down target e1 id, go_down target e2 id)
  in match e with*)
      
(*
Evaluation via substitution *)

module Subst = struct

let rec subst (x : ident) (s : expr) (e : expr) : expr =
  match e with
  | Binop (op, e1, e2) -> Binop (op, subst x s e1, subst x s e2)
  | If (p, t, e) -> If (subst x s p, subst x s t, subst x s e)
  | Var y -> if x = y
               then s
               else e
  | Let (y, e1, e2) -> if x = y
                         then Let (y, subst x s e1, e2)
                         else Let (y, subst x s e1, subst x s e2)
  | _ -> e
  
let expr_of_value (v : value) : expr =
  match v with
  | VInt a -> Int a
  | VBool a -> Bool a

let rec eval (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (p, t, e) ->
      (match eval p with
      | VBool true -> eval t
      | VBool false -> eval e
      | _ -> failwith "type error")
  | Binop (And, e1, e2) ->
      (match eval e1 with
      | VBool true -> eval e2
      | VBool false -> VBool false
      | _ -> failwith "type error")
  | Binop (Or, e1, e2) ->
      (match eval e1 with
      | VBool false -> eval e2
      | VBool true -> VBool true
      | _ -> failwith "type error")
  | Binop (op, e1, e2) -> eval_op op (eval e1) (eval e2)
  | Let (x, e1, e2) -> let s = expr_of_value (eval e1) in
                       eval (subst x s e2)
  | Var x -> failwith ("unbound value " ^ x)

let interp (s : string) : value =
  eval (parse s)

end


(* Evaluation via environments *)

module Env = struct

module M = Map.Make(String)

type env = value M.t

let rec eval_env (env : env) (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (p, t, e) ->
      (match eval_env env p with
      | VBool true -> eval_env env t
      | VBool false -> eval_env env e
      | _ -> failwith "type error")
  | Binop (And, e1, e2) ->
      (match eval_env env e1 with
      | VBool true -> eval_env env e2
      | VBool false -> VBool false
      | _ -> failwith "type error")
  | Binop (Or, e1, e2) ->
      (match eval_env env e1 with
      | VBool false -> eval_env env e2
      | VBool true -> VBool true
      | _ -> failwith "type error")
  | Binop (op, e1, e2) -> eval_op op (eval_env env e1) (eval_env env e2)
  | Let (x, e1, e2) ->
      let r = eval_env env e1 in
      let new_env = M.update x (fun _ -> Some r) env in
      eval_env new_env e2
  | Var x ->
      (match M.find_opt x env with
      | Some v -> v
      | None -> failwith ("unbound value" ^ x))

let eval : expr -> value = eval_env M.empty 

let interp (s : string) : value =
  eval (parse s)

end

