let build_list f n = 
  let rec it f n acc = 
    if n == 0 then acc else it f (n-1) ( f(n-1) :: acc) in
  it f n [] 
;;

let negatives n = build_list (fun x -> -x ) n

let reciprocals n = build_list (fun x -> 1. /. float_of_int x) n

let evens n = build_list (fun x -> 2*x) n

let identityM n = build_list (fun x -> 
  let rec gen p n acc =
    if n == 0 then acc else gen p (n-1) ((if n-1 == p then 1 else 0) :: acc) in
  gen x n [] ) 
  n
;;
identityM 10


;;