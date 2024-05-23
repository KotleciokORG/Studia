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



module MakeMapDict (M : Map.OrderedType): DICT with type key = M.t = struct
  type key = M.t
  
  module Map = Map.Make(M)

  type 'a dict = 'a Map.t
  let empty = Map.empty
  let remove k d = Map.filter (fun  k' _ -> M.compare k k' <> 0) d
  let insert k v d = Map.add k v d
  let find_opt k d = Map.find_opt k d
  let find k d = Map.find k d
  let to_list d = Map.to_list d
end

module  CharMapDict = MakeMapDict (
  struct 
    type t = char
    let compare = Char.compare 
  end
)