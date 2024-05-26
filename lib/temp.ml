open Symbol
open Base

module type TEMP = sig
  (* Abstract names for local variables *)
  type temp [@@deriving compare, sexp]

  (* Returns a new temporary from an infinite set of temps *)
  val new_temp : unit -> temp
  val make_string : temp -> string

  (* Abstract names for static memory addresses *)
  type label [@@deriving compare, sexp]

  (* Returns a new label from an infinite set of labels *)
  val new_label : unit -> label

  (* Returns a new label whose assembly-language name is
     what's passed in *)
  val named_label : string -> label
  val label_to_string : label -> string

  (* Table of temps *)
  type 'a tbl

  val empty : 'a tbl
  val enter : 'a tbl * temp * 'a -> 'a tbl
  val look : 'a tbl * temp -> 'a option
  val reset : unit -> unit

  module Set : Caml.Set.S
end

module Temp = struct
  (* Abstract names for local variables *)
  type temp = int [@@deriving compare, sexp]

  let temps = ref 100

  (* Returns a new temporary from an infinite set of temps *)
  let new_temp () =
    let t = !temps in
    temps := t + 1;
    t

  let make_string t = "t" ^ Int.to_string t

  type label = Symbol.symbol [@@deriving compare, sexp]

  let postinc x =
    let i = !x in
    x := i + 1;
    i

  let labs = ref 0

  (* Returns a new label from an infinite set of labels *)
  let new_label () = Symbol.to_symbol (Printf.sprintf "L%d" (postinc labs))

  (* Returns a new label whose assembly-language name is
     what's passed in *)
  let named_label = Symbol.to_symbol
  let label_to_string = Symbol.name

  (* Map that has an Int key (i.e. same type as temp) and 'a value *)
  module IntMap = Caml.Map.Make (Int)

  (* Set of temporaries *)
  module Set = Caml.Set.Make (Int)

  type 'a tbl = 'a IntMap.t

  let empty = IntMap.empty
  let enter (t, temp, v) = IntMap.add temp v t
  let look (t, temp) = IntMap.find_opt temp t

  let reset () =
    (* Just so that we have enough labels for initial stuff *)
    labs := 50;
    temps := 130
end
