type 'v nnf =
| NNFLit of bool * 'v
| NNFConj of 'v nnf * 'v nnf
| NNFDisj of 'v nnf * 'v nnf
;;
(*eval_nnf typu ('a -> bool) -> 'a nnf -> bool*)

let rec eval_nnf sigma fi = match fi with
| NNFLit (a, psi) -> if a = false then not (sigma a) else sigma a
| NNFConj (phi, psi) -> eval_nnf sigma phi && eval_nnf sigma psi
| NNFDisj (phi, psi) -> eval_nnf sigma phi || eval_nnf sigma psi

