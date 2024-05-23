open Ast

let parse (s : string) : expr =
  Parser.prog Lexer.read (Lexing.from_string s)

type value =
  | VInt of int
  | VBool of bool

let closed (e : expr) : bool = 
  let rec check (e : expr) (bound : ident list) : bool =   
    match e with
    | Int _ -> true
    | Bool _ -> true
    | Var v -> if ((List.find_opt (fun x -> x = v) bound) = Some v ) then true else false
    | Binop (_ , e1 , e2) -> check e1 bound && check e2 bound (*moze nawiasy*)
    | If (e1 , e2 , e3) -> check e1 bound && check e2 bound && check e3 bound
    | Let (id , e1 , e2) -> check e1 bound && check e2 (id::bound)
    | _ -> false
  in check e []


let alpha_equiv (e1 : expr) (e2 : expr) : bool  =
  let rec doit (e1 : expr) (e2 : expr) (l1 : ident list) (l2 : ident list) : bool= 
    match e1, e2 with
    | Int a, Int b -> a = b
    | Bool a, Bool b -> a = b
    | Var a, Var b -> (List.find_index (fun x -> x = a) l1) = (List.find_index (fun x -> x = b) l2)
    | Binop (op1 , p1 , p2) , Binop (op2 , q1 , q2)  -> op1 = op2 && doit p1 q1 l1 l2 && doit p2 q2 l1 l2
    | If (p1,p2,p3) , If (q1,q2,q3) -> doit p1 q1 l1 l2 && doit p2 q2 l1 l2 && doit p3 q3 l1 l2
    | Let (id1,p1,p2) , Let (id2,q1,q2) -> let new_l1 = id1::l1 and new_l2 = id2::l2 in 
                                            doit p1 q1 l1 l2 && doit p2 q2 new_l1 new_l2
    | _ , _ -> false
  in doit e1 e2 [] []





let rename_expr (e : expr) : expr =
  let rec doit (e : expr) (dict : (ident * ident) list) (aktident : ident): expr = 
    let rec find_value (key : ident) (dict : (ident * ident) list) : ident option = 
      match dict with
      | [] -> None
      | (id1,id2) :: tail -> if id1 = key then Some id2 else find_value key tail
    in
    match e with 
    | Int _ -> e
    | Bool _ -> e
    | Var a -> (match find_value a dict with 
                | None -> Var a
                | Some b -> Var b)
    | Binop (op, e1, e2) -> Binop(op, doit e1 dict (aktident^"L"), doit e2 dict (aktident^"R"))
    | If (p1, p2, p3) -> If (doit p1 dict (aktident^"L") , doit p2 dict (aktident^"M") , doit p3 dict (aktident^"R"))
    | Let (id, e1, e2) -> Let ( aktident , doit e1 ((id,aktident)::dict) (aktident^"L") , doit e2 ((id,aktident)::dict) (aktident^"R"))
    | _ -> e
  in doit e [] "#" 


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






(* Evaluation via substitution *)

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
  | Sum (y, e1, e2, e3) -> if y = x 
                              then Sum (y, subst x s e1, subst x s e2, e3) (*pierwsze dwa zawsze subst bo tam nie siega y z ident*)
                              else Sum (y, subst x s e1, subst x s e2, subst x s e3) (*ostatni tylko gdy nie nadpisujemy identyfikatora*)
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
  | Sum (id, e1, e2, e3) -> let rec suma it stop acc = 
                              match (eval (Binop (Eq,it,stop))) with 
                              | VBool true -> acc
                              | VBool false -> suma 
                                                (Binop(Add,it,Int 1))
                                                stop 
                                                (eval(Binop(Add,expr_of_value acc, subst id it e3)))
                              | VInt _ -> failwith "type error"
                            in suma e1 e2 (VInt 0) 

                                

let interp (s : string) : value =
  eval (parse s)

end


(* Evaluation via environments *)

module Env = struct

module M = Map.Make(String)

type env = value M.t

let expr_of_value (v : value) : expr =
  match v with
  | VInt a -> Int a
  | VBool a -> Bool a

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
  | Sum (id, e1, e2, e3) -> let rec suma it stop acc = 
      match (eval_env env (Binop (Eq,it,stop))) with 
      | VBool true -> acc
      | VBool false -> 
        let new_env = M.update id (fun _ -> Some (eval_env env it)) env in
          suma 
            (Binop(Add,it,Int 1))
            stop 
            (Binop(Add, acc, expr_of_value (eval_env new_env e3)))
      | VInt _ -> failwith "type error"
    in eval_env env (suma e1 e2 (Int 0) )

let eval : expr -> value = eval_env M.empty 

let interp (s : string) : value =
  eval (parse s)

end

