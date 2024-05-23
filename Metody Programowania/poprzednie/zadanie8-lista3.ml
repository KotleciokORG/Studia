type 'a tree = Leaf | Node of  'a tree * int * 'a tree;;

let rec insert_bst (a : 'a) (t : 'a tree) = 
  match t with
  | Leaf -> Node (Leaf, a ,Leaf)
  | Node (l,v,p) -> if v<=a then Node (l,v,(insert_bst a p)) else Node ((insert_bst a l),v,p)
;;
let t =
  Node ( Node ( Leaf , 2, Leaf ) ,
    5,
    Node (( Node ( Leaf , 6, Leaf )) ,
  8,
  ( Node ( Leaf , 9, Leaf ))))
;;

let a = [4;3;6;1;22;2;2;2;10;9];;
let temp = Leaf;;
let list_to_tree xs = 
  let rec it xs acc = 
    match xs with
    | [] -> acc
    | x::xs' -> it xs' (insert_bst x acc)
  in it xs Leaf
;;

let rec flat_append t xs = 
  match t with
  |Leaf -> xs
  |Node(l,v,p) -> flat_append l (v::(flat_append p xs))
;;

let tree_sort xs = 
  flat_append (list_to_tree xs) []
;;
