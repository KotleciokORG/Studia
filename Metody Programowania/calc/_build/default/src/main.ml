open Ast

let parse (s : string) : expr =
  Parser.prog Lexer.read (Lexing.from_string s)

type value =
  | VInt of int
  | VBool of bool

let eval_op (op : bop) (v1 : value) (v2 : value) : value =
  let mod5 x = ((x mod 5) + 5) mod 5 in
  match op, v1, v2 with
  | Add,  VInt i1, VInt i2 -> VInt (mod5(i1 + i2))
  | Sub,  VInt i1, VInt i2 -> VInt (mod5(i1 - i2))
  | Mult, VInt i1, VInt i2 -> VInt (mod5(i1 * i2))
  | Div,  VInt i1, VInt i2 -> VInt (mod5(i1 / i2))
  | Eq,   VInt i1, VInt i2 -> VBool (i1 = i2)
  | _ -> failwith "type error"

let rec eval (e : expr) : value =
  match e with
  | Int n -> VInt (((n mod 5)+5) mod 5)
  | Bool b -> VBool b
  | If (p, t, e) ->
      (match eval p with
      | VBool true -> eval t
      | VBool false -> eval e
      | _ -> failwith "type error") 
  | Binop (op, e1, e2) -> eval_op op (eval e1) (eval e2)

let interp (s : string) : value =
  eval (parse s)

