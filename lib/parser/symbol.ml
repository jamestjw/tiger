module type SYMBOL = sig
  type symbol

  val to_symbol : string -> symbol
  val name : symbol -> string
  val compare_symbol : symbol -> symbol -> int

  type 'a table

  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val look : 'a table * symbol -> 'a option
end

module Symbol : SYMBOL = struct
  type symbol = string * int

  let sizeHint = 128

  module H = Hashtbl

  let hashtable = H.create sizeHint
  let nextsym = ref 0

  let to_symbol name =
    let i =
      try H.find hashtable name
      with Not_found ->
        let i = !nextsym in
        nextsym := i + 1;
        H.add hashtable name i;
        i
    in
    (name, i)

  (* Just return the first part of the tuple which is the symbol name*)
  let name (s, _) = s
  let compare_symbol s1 s2 = String.compare (name s1) (name s2)

  (* Map that has an Int key and 'a value *)
  module IntMap = Map.Make (Int)

  type 'a table = 'a IntMap.t

  let empty = IntMap.empty
  let enter (t, (_, s_i), v) = IntMap.add s_i v t
  let look (t, (_, s_i)) = IntMap.find_opt s_i t
end
