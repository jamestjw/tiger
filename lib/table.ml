(* module type TABLE= sig
     type key
     type 'a tbl

     val empty : 'a tbl
     val enter : 'a tbl * key * 'a -> 'a tbl
     val look : 'a tbl * key -> 'a option
   end *)

module type KEY = sig
  type t

  val getKey : t -> int
end

module IntMapTable (Key : KEY) = struct
  module IntMap = Map.Make (Int)

  type 'a tbl = 'a IntMap.t
  type key = Key.t

  let empty = IntMap.empty
  let enter (t, k, v) = IntMap.add (Key.getKey k) v t
  let look (t, k) = IntMap.find_opt (Key.getKey k) t
  let bindings = IntMap.bindings
  let map = IntMap.map
  let fold = IntMap.fold
end
