open Base
open Errormsg

module type SYMBOL = sig
  type symbol [@@deriving compare, sexp, equal]

  val to_symbol : string -> symbol
  val name : symbol -> string

  type 'a tbl

  val empty : 'a tbl
  val enter : 'a tbl * symbol * 'a -> 'a tbl
  val look : 'a tbl * symbol -> 'a option
end

module Symbol : SYMBOL = struct
  type symbol = string * int [@@deriving compare, sexp, equal]

  module H = Hashtbl

  let hashtable = H.create (module String)
  let nextsym = ref 0

  let to_symbol name =
    let i =
      match H.find hashtable name with
      | Some i' -> i'
      | None ->
          let i = !nextsym in
          nextsym := i + 1;
          (match H.add hashtable ~key:name ~data:i with
          | `Ok -> ()
          | `Duplicate -> ErrorMsg.impossible "Duplicate symbol");
          i
    in
    (name, i)

  (* Just return the first part of the tuple which is the symbol name*)
  let name (s, _) = s
  let compare_symbol s1 s2 = String.compare (name s1) (name s2)

  (* Map that has an Int key and 'a value *)
  module IntMap = Caml.Map.Make (Int)

  type 'a tbl = 'a IntMap.t

  let empty = IntMap.empty
  let enter (t, (_, s_i), v) = IntMap.add s_i v t
  let look (t, (_, s_i)) = IntMap.find_opt s_i t
end
