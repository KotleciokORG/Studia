module type DICT = sig
  type key
  type 'a dict
  val empty : 'a dict
  val insert : key -> 'a -> 'a dict -> 'a dict
  val remove : key -> 'a dict -> 'a dict
  val find_opt : key -> 'a dict -> 'a option
  val find : key -> 'a dict -> 'a
  val to_list : 'a dict -> (key * 'a) list
end
(*M.compare k k'  <> 0*)

module MakeListDict (M : Map.OrderedType): DICT with type key = M.t = struct
  type key = M.t
  type 'a dict = (key * 'a) list
  let empty = []
  let remove k d = List.filter (fun (k', _) -> M.compare k k' <> 0) d
  let insert k v d = (k, v) :: remove k d
  let find_opt k d = List.find_opt (fun (k', _) -> M.compare k k' = 0) d |> Option.map snd  
  let find k d = List.find (fun (k', _) -> M.compare k k' = 0) d |> snd
  let to_list d = d
end

module CharListDict = MakeListDict (
  struct 
    type t = char
    let compare = Char.compare 
  end
)