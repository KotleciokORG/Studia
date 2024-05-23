open Ast

module M = Map.Make(String)

exception Type_error
exception Unbound_var of ident

exception MyExn

type env = value M.t

and value =
  | VInt of int
  | VBool of bool
  | VClosure of ident * expr * env
  | VPair of value * value

type comp =
  | Value of value
  | Exn of ident
  | TypeErro


let ( let* ) m f =
  match m with
  | Exn(x) -> Exn(x)
  | Value v -> f v
  | TypeErro -> TypeErro

let return x = Value x

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
  | _ -> raise Type_error

let rec eval_env (env : env) (e : expr) : comp =
  match e with
  | Int n -> return (VInt n)
  | Bool b -> return (VBool b)
  | If (p, t, e) ->
      let* c = eval_env env p in
      (match c with
      | VBool true -> eval_env env t
      | VBool false -> eval_env env e
      | _ -> TypeErro)
  | Binop (And, e1, e2) ->
      let* c = eval_env env e1 in
      (match c with
      | VBool true -> eval_env env e2
      | VBool false -> return (VBool false)
      | _ -> TypeErro)
  | Binop (Or, e1, e2) ->
      let* c = eval_env env e1 in
      (match c with
      | VBool false -> eval_env env e2
      | VBool true -> return (VBool true)
      | _ -> TypeErro)
  | Binop (op, e1, e2) ->
      let* v1 = eval_env env e1 in
      let* v2 = eval_env env e2 in
      (
        try return (eval_op op v1 v2) with Type_error -> TypeErro
        )
  | Let (x, e1, e2) ->
      let* r = eval_env env e1 in
      let new_env = M.add x r env in
      eval_env new_env e2
  | Var x ->
      return
        (match M.find_opt x env with
        | Some v -> v
        | None -> raise (Unbound_var x))
  | Fun (x, e) -> return (VClosure (x, e, env))
  | App (e1, e2) ->
      (* ( let* ) (eval_env env e1) (fun v1 -> ... ) *)
      let* v1 = eval_env env e1 in
      let* v2 = eval_env env e2 in
      (match v1 with
      | VClosure (x, body, clo_env) ->
        eval_env (M.add x v2 clo_env) body
      | _ -> TypeErro)
  | Pair(e1, e2) ->
      let* v1 = eval_env env e1 in
      let* v2 = eval_env env e2 in
      return (VPair(v1, v2))
  | Fst e ->
      let* v = eval_env env e in
      (match v with
      | VPair(v1, _) -> return v1
      | _ -> TypeErro)
  | Snd e ->
      let* v = eval_env env e in
      (match v with
      | VPair(_, v2) -> return v2
      | _ -> TypeErro)
  | Raise (id) -> Exn(id)
  | Try(e1, try_id, e2) ->
      (match eval_env env e1 with
      | Exn ex_id -> if ex_id = try_id then eval_env env e2 else Exn ex_id
      | TypeErro -> eval_env env e2
      | Value v -> return v)

let eval_prog p =
  match eval_env M.empty p with
  | Exn(_) -> raise MyExn
  | TypeErro -> raise Type_error
  | Value v -> v

let rec string_of_value v =
  match v with
  | VInt n      -> string_of_int n
  | VBool true  -> "true"
  | VBool false -> "false"
  | VClosure _  -> "<fun>"
  | VPair(v1, v2) ->
    "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"

let print_value v =
  print_endline (string_of_value v)
