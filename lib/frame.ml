open Temp
open Base

module type FRAME = sig
  type frame
  type access [@@deriving compare, sexp]
  type new_frame_args = { name : Temp.label; formals : bool list }

  val new_frame : new_frame_args -> frame
  val name : frame -> Temp.label

  (* Extracts a list of k "accesses" denoting the locations
     where the formal parameters will be kept at run time,
     as seen from inside the callee *)
  val formals : frame -> access list
  val alloc_local : frame -> bool -> access
end

module X86Frame : FRAME = struct
  type access = InFrame of int | InReg of Temp.temp [@@deriving compare, sexp]

  type frame = {
    name : Temp.label;
    formals : bool list;
    locals : access list ref;
    next_local_offset : int ref;
  }

  type new_frame_args = { name : Temp.label; formals : bool list }

  (* TODO: Make this variable *)
  let word_size = 8
  let formals_start_offset = 16

  let new_frame { name; formals } =
    { name; formals; locals = ref []; next_local_offset = ref (-8) }

  let name (f : frame) = f.name

  (* TODO: Complete this, for now assume that all formals are escaped *)
  let formals (f : frame) =
    let _, l =
      List.fold_left
        ~f:(fun (pos, l) _formal -> (pos + word_size, InFrame pos :: l))
        ~init:(formals_start_offset, []) f.formals
    in
    List.rev l

  let alloc_local f is_escape =
    if is_escape then (
      let res = InFrame !(f.next_local_offset) in
      f.next_local_offset := !(f.next_local_offset) - word_size;
      res)
    else InReg (Temp.new_temp ())

  let%test_unit "test_all_escape_formals" =
    let frame =
      new_frame
        { name = Temp.named_label "frame_name"; formals = [ true; true; true ] }
    in
    [%test_eq: access list] (formals frame)
      [ InFrame 16; InFrame 24; InFrame 32 ]

  let%test_unit "test_all_escape_locals" =
    let frame =
      new_frame { name = Temp.named_label "frame_name"; formals = [] }
    in
    let expected = [ InFrame (-8); InFrame (-16); InFrame (-24) ] in
    [%test_eq: access list]
      (List.map ~f:(fun _ -> alloc_local frame true) expected)
      expected
end

module Frame : FRAME = X86Frame
