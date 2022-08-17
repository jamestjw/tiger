open Temp
open Frame

module type TRANSLATE = sig
  type exp
  type level
  type access (* Not the same as Frame.access *)

  type new_level_args = {
    parent : level;
    name : Temp.label;
    formals : bool list;
  }

  val outermost : level
  val new_level : new_level_args -> level
  val formals : level -> access list
  val alloc_local : level -> bool -> access
  val default_exp : exp
end

module Translate : TRANSLATE = struct
  type exp = unit
  type level = { depth : int; frame : Frame.frame }
  type access = level * Frame.access

  type new_level_args = {
    parent : level;
    name : Temp.label;
    formals : bool list;
  }

  let outermost =
    {
      depth = 0;
      frame = Frame.new_frame { name = Temp.named_label "main"; formals = [] };
    }

  let new_level { name; parent; formals } =
    {
      depth = parent.depth + 1;
      (* Add an extra parameter that escapes to represent the static link *)
      frame = Frame.new_frame { name; formals = true :: formals };
    }

  let formals l =
    List.map
      (fun f -> (l, f))
      (* Skip the first formal as that is the static link *)
      (List.tl (Frame.formals l.frame))

  let alloc_local l escape = (l, Frame.alloc_local l.frame escape)
  let default_exp = ()
end
