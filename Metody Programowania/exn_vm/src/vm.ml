open Ast

type cmd =
  | PushInt of int
  | PushBool of bool
  | Prim of bop
  | CondJmp of cmd list * cmd list
  | Grab           (* wstaw wartosc ze stosu do srodowiska *)
  | Access of int  (* wstaw wartosc ze srodowiska na stos *)
  | EndLet
  | PushClo of cmd list
  | Call
  | Return
  | Raise
  | BeginTry of cmd list * cmd list
  | EndTry
            
type vm_value =
  | VMInt of int
  | VMBool of bool
  | VMClosure of cmd list * vm_value list
  | VMRetAddr of cmd list * vm_value list

let eval_vm_op (op : bop) (v1 : vm_value) (v2 : vm_value) : vm_value =
  match op, v1, v2 with
  | Add,  VMInt i1, VMInt i2 -> VMInt (i1 + i2)
  | Sub,  VMInt i1, VMInt i2 -> VMInt (i1 - i2)
  | Mult, VMInt i1, VMInt i2 -> VMInt (i1 * i2)
  | Div,  VMInt i1, VMInt i2 -> VMInt (i1 / i2)
  | Eq,   VMInt i1, VMInt i2 -> VMBool (i1 = i2)
  | Lt,   VMInt i1, VMInt i2 -> VMBool (i1 < i2)
  | Gt,   VMInt i1, VMInt i2 -> VMBool (i1 > i2)
  | Leq,  VMInt i1, VMInt i2 -> VMBool (i1 <= i2)
  | Geq,  VMInt i1, VMInt i2 -> VMBool (i1 >= i2)
  | Neq,  VMInt i1, VMInt i2 -> VMBool (i1 <> i2)
  | _ -> failwith "type error"

let rec exec (p : cmd list) (s : vm_value list) (env : vm_value list) hstack1 : vm_value =
  match p, s with
  | [], [v] -> v
  | PushInt n :: p', _ -> exec p' (VMInt n :: s) env hstack1
  | PushBool b :: p', _ -> exec p' (VMBool b :: s) env hstack1
  | Prim op :: p', (v1 :: v2 :: s) -> exec p' (eval_vm_op op v2 v1 :: s) env hstack1
  | CondJmp (t, e) :: p', VMBool v :: s' -> if v then exec (t @ p') s' env hstack1
                                            else exec (e @ p') s' env hstack1
  | Grab :: p', v :: s' -> exec p' s' (v :: env) hstack1
  | Access n :: p', _ -> exec p' (List.nth env n :: s) env hstack1
  | EndLet :: p', _ -> exec p' s (List.tl env) hstack1
  | PushClo q :: p', _ -> exec p' (VMClosure (q, env) :: s) env hstack1
  | Call :: p', VMClosure (q, env') :: v :: s' ->
     exec q (VMRetAddr (p', env) :: s') (v :: env') hstack1
  | Return :: _, v :: VMRetAddr (p, env') :: s' -> exec p (v :: s') env' hstack1
  | BeginTry(p1, q) :: p', _ ->
    exec (p1 @ p') s env ((q @ p', s, env) :: hstack1)
  | EndTry :: p', _ ->
    exec p' s env (List.tl hstack1)
  | Raise :: _, _ ->
    (match hstack1 with
    | [] -> failwith "unhandled exception"
    | (q, s', env') :: hst ->
      exec q s' env' hst)
  | _, _ -> failwith "error"
       
let exec_prog p = exec p [] [] []

let print_value v =
  print_endline
    (match v with
    | VMInt  n     -> string_of_int n
    | VMBool true  -> "true"
    | VMBool false -> "false"
    | VMClosure _  -> "<fun>"
    | VMRetAddr _  -> "<ret-addr>")
