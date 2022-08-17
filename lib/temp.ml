open Symbol
open Base

module type TEMP = sig
  (* Abstract names for local variables *)
  type temp [@@deriving compare, sexp]

  (* Returns a new temporary from an infinite set of temps *)
  val new_temp : unit -> temp
  val make_string : temp -> string

  (* Abstract names for static memory addresses *)
  type label

  (* Returns a new label from an infinite set of labels *)
  val new_label : unit -> label

  (* Returns a new label whose assembly-language name is
     what's passed in *)
  val named_label : string -> label
end

module Temp : TEMP = struct
  type temp = int [@@deriving compare, sexp]

  let temps = ref 100

  let new_temp () =
    let t = !temps in
    temps := t + 1;
    t

  let make_string t = "t" ^ Int.to_string t

  type label = Symbol.symbol

  let postinc x =
    let i = !x in
    x := i + 1;
    i

  let labs = ref 0
  let new_label () = Symbol.to_symbol (Printf.sprintf "L%d" (postinc labs))
  let named_label = Symbol.to_symbol
end
