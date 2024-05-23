type 'a tree = Leaf | Node of  'a tree * int * 'a tree;;

let t =
  Node ( Node ( Leaf , 2, Leaf ) ,
    5,
    Node (( Node ( Leaf , 6, Leaf )) ,
  8,
  ( Node ( Leaf , 9, Leaf ))))
;;

(* let flatten t = fold_tree (fun l v r -> l @ [v] @ r) [] t *)

let rec flat_append t xs = 
  match t with
  |Leaf -> xs
  |Node(l,v,p) -> flat_append l (v::(flat_append p xs))
;;

let flatten t =   
  flat_append t []
;; 

