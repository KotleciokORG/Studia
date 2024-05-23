(*It will no be used, but I will refer to it and so it's still here*)
let ( let* ) xs ys = List.concat_map ys xs

(*Check if the row is full of zeros*)
let rec is_zero xs = 
  match xs with
  |[] -> true
  |0 :: xs' -> is_zero xs'
  |_ :: _  -> false

(*Make row full of falses*)
let rec make_false_table n = 
  if n = 0 then [] else false :: (make_false_table (n-1))

let build_row ps n =
  if is_zero ps then [make_false_table n] else (*Slightly improves time*)
  let rec fin xs left acc =
    if left = 0 then 
    match xs with
    |[] -> [acc]
    |0::ys -> fin ys left acc
    |_::_ -> []
    else
    match xs with (*If 1 true to be placed, then make a space for another block, if 0 then try to start it now or later*)
    | [] ->  (fin [] (left-1) (false::acc))
    | 1 :: ys -> if left = 1 then fin (0::ys) (left-1) (true::acc) else fin (0::ys) (left-2) (false::true::acc)
    | 0 :: ys -> if List.length ys = 0 then fin [] left acc else (fin ys left acc) @ (fin xs (left-1) (false::acc))
    | a :: ys -> (fin ((a-1)::ys) (left-1) (true::acc) )
  in (fin (0::ps) n []) |> List.map (fun row -> List.rev row)


let build_candidate pss n =
  let rec fin xss acc = 
    match xss with
    |[] -> acc
    |form::xss' -> fin xss' (List.concat_map (fun row -> (List.map (fun gra -> row :: gra) acc)) (build_row form n))
                            (*Here I think that the concat_map is more readable than let* *)
  in fin pss [[]] |> List.map (fun game -> List.rev game)
  



let verify_row ps xs =
  let rec it form row flag_inside = 
    match row with
    | [] -> if form != [] then false else true
    | a :: row' -> if a = false then if flag_inside = true then false else it form row' false else 
      match form with
      | [] -> false
      | 0 :: form' -> it form' row false
      | 1 :: form' -> it form' row' false
      | a :: form' -> it ((a-1)::form') row' true
  in it ps xs false
  
  
let verify_rows pss xss =
  let rec it forms rows = 
    match (forms,rows) with
    |([],[]) -> true
    |(form::forms',row::rows') -> if (verify_row form row) = true then it forms' rows' else false
    |(_,_) -> false (*It should not happen, but I left it here because of OCaml warnings*)
  in it pss xss
    
let transpose xss = 
  let rec tr lst =
    match lst with
    | [] -> []
    | []::_ -> []
    | _ ->
        List.map List.hd lst :: tr (List.map List.tl lst)
    in tr xss


type nonogram_spec = { rows : int list list ; cols : int list list }

let solve_nonogram nono =
  build_candidate (nono.rows) (List.length (nono.cols))
  |> List.filter (fun xss -> transpose xss |> verify_rows nono.cols)
      
let example_1 = {
  rows = [[2];[1];[1]];
  cols = [[1;1];[2]]
}
  
let example_2 = {
  rows = [[2];[2;1];[1;1];[2]];
  cols = [[2];[2;1];[1;1];[2]]
}
let big_example = {
  rows = [[1;2];[2];[1];[1];[2];[2;4];[2;6];[8];[1;1];[2;2]];
  cols = [[2];[3];[1];[2;1];[5];[4];[1;4;1];[1;5];[2;2];[2;1]]
}
          

