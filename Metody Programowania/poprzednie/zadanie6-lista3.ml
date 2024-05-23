type 'a tree = Leaf | Node of  'a tree * int * 'a tree;;

let t =
  Node ( Node ( Leaf , 2, Leaf ) ,
    5,
    Node (( Node ( Leaf , 6, Leaf )) ,
  8,
  ( Node ( Leaf , 9, Leaf ))))
;;
let rec fold_tree f pass t = 
  match t with
  | Leaf -> pass
  | Node (l,v,p) -> f (fold_tree f pass l) v (fold_tree f pass p)
;;


let tree_product t = 
  fold_tree (fun a b c -> a*b*c ) 1 t
;;
let tree_flip t = 
  fold_tree (fun (a:'a tree) b (c:'a tree) -> Node(c, b, a)) Leaf t
;;

let tree_height t = 
  fold_tree (fun a b c -> max (a+1) (c+1)) 0 t
;;
let tree_span t = 
  fold_tree (fun a b c -> let temp = (let (t1, _) = a and (_, t2) = c in (t1, t2)) 
                          in if temp = (0,0) then (b,b) else temp) (0,0) t
;;

let preorder t = 
  fold_tree (fun a b c -> [b] @ a @ c) [] t