open Temp
open Base
open Tree

module type FRAME = sig
  type frame
  type access [@@deriving compare, sexp]
  type new_frame_args = { name : Temp.label; formals : bool list }

  (* Fragments that are processed at the beginning of code generation,
     usually for statics and function definitions *)
  type frag =
    | PROC of { body : Tree.stm; frame : frame }
    | STRING of Temp.label * string

  val new_frame : new_frame_args -> frame
  val name : frame -> Temp.label

  (* Extracts a list of k "accesses" denoting the locations
     where the formal parameters will be kept at run time,
     as seen from inside the callee *)
  val formals : frame -> access list
  val alloc_local : frame -> bool -> access

  (* The expression that is passed in is the address of the stack frame
     that the access lives in*)
  val exp : access -> Tree.exp -> Tree.exp
  val word_size : int
  val fp : Temp.temp
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

  type frag =
    | PROC of { body : Tree.stm; frame : frame }
    | STRING of Temp.label * string

  (* TODO: Make this variable *)
  let word_size = 8
  let formals_start_offset = 16
  let num_formals_in_registers = 6

  let new_frame { name; formals } =
    { name; formals; locals = ref []; next_local_offset = ref (-8) }

  let name (f : frame) = f.name

  let formals (f : frame) =
    let _, _, l =
      List.fold_left
        ~f:(fun (i, pos, l) escape ->
          if i < num_formals_in_registers && not escape then
            (i + 1, pos, InReg (Temp.new_temp ()) :: l)
          else (i + 1, pos + word_size, InFrame pos :: l))
        ~init:(0, formals_start_offset, [])
        f.formals
    in
    List.rev l

  let alloc_local f is_escape =
    if is_escape then (
      let res = InFrame !(f.next_local_offset) in
      f.next_local_offset := !(f.next_local_offset) - word_size;
      res)
    else InReg (Temp.new_temp ())

  let fp = Temp.new_temp ()

  let exp a e =
    match a with
    | InFrame k -> Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST k))
    | InReg r -> Tree.TEMP r

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
