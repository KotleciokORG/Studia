type 'a tree = Leaf | Node of  'a tree * int * 'a tree;;

let rec insert_bst (a : 'a) (t : 'a tree) = 
  match t with
  | Leaf -> Node (Leaf, a ,Leaf)
  | Node (l,v,p) -> if v<a then Node (l,v,(insert_bst a p)) else Node ((insert_bst a l),v,p)
;;
let t =
  Node ( Node ( Leaf , 2, Leaf ) ,
    5,
    Node (( Node ( Leaf , 6, Leaf )) ,
  8,
  ( Node ( Leaf , 9, Leaf ))))