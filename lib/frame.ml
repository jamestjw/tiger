open Assem
open Base
open Symbol
open Temp
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
  val alloc_local : bool -> int -> access

  (* The expression that is passed in is the address of the stack frame
     that the access lives in*)
  val exp : access -> Tree.exp -> Tree.exp
  val word_size : int

  (* Special registers:
     1. Frame pointer
     2. Return value
     3. Return address *)
  val fp : Temp.temp
  val rv : Temp.temp
  val ra : Temp.temp
  val sp : Temp.temp
  val externalCall : string * Tree.exp list -> Tree.exp

  (* Handles the view shift by
     1. Moving incoming formals
     2. Saving and restoring callee-save registers *)
  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

  type fn_prolog_epilog = {
    prolog : string;
    body : Assem.instr list;
    epilog : string;
  }

  val procEntryExit3 : frame * Assem.instr list -> fn_prolog_epilog

  type register

  val special_regs : Temp.temp list
  val arg_regs : Temp.temp list
  val callee_saves : Temp.temp list
  val caller_saves : Temp.temp list
  val registerToString : Temp.temp -> string
end

(* TODO: Implement this if we really want to target the x86 architecture *)
(* module X86Frame : FRAME = struct
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
     let rv = Temp.new_temp ()

     let exp a e =
       match a with
       | InFrame k -> Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST k))
       | InReg r -> Tree.TEMP r

     let externalCall (s, args) = Tree.CALL (Tree.NAME (Temp.named_label s), args)

     (* TODO: Implement this *)
     let procEntryExit1 (_, stm) = stm

     type register = string

     let tempMap = Temp.empty
     let special_regs = []
     let arg_regs = []
     let callee_saves = []
     let caller_saves = []

     let registerToString temp =
       match Temp.look (tempMap, temp) with
       | Some r -> r
       | None -> Temp.make_string temp

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
   end *)

(* https://riscv.org/wp-content/uploads/2015/01/riscv-calling.pdf *)
module RiscVFrame : FRAME = struct
  type access = InFrame of int | InReg of Temp.temp [@@deriving compare, sexp]

  type frame = {
    name : Temp.label;
    formals : bool list;
    locals : access list ref;
  }

  type new_frame_args = { name : Temp.label; formals : bool list }

  type frag =
    | PROC of { body : Tree.stm; frame : frame }
    | STRING of Temp.label * string

  let word_size = 8

  (* The stack pointer points to the first argument not passed in a register *)
  let formals_start_offset = 0

  (* First position in the stack seems to be -24(s0) *)
  let local_start_offset = -24

  (* Args go in a0 - a7 *)
  let num_formals_in_registers = 8
  let new_frame { name; formals } = { name; formals; locals = ref [] }
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

  let alloc_local is_escape local_num =
    if is_escape then InFrame (local_start_offset - (word_size * local_num))
    else InReg (Temp.new_temp ())

  let fp = Temp.new_temp ()
  let rv = Temp.new_temp ()
  let ra = Temp.new_temp ()
  let sp = Temp.new_temp ()

  let exp a e =
    match a with
    | InFrame k -> Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST k))
    | InReg r -> Tree.TEMP r

  let externalCall (s, args) = Tree.CALL (Tree.NAME (Temp.named_label s), args)

  (* TODO: Implement this *)
  let procEntryExit1 (_, stm) = stm

  type register = string [@@deriving compare, sexp]

  (* TODO: Is this the only way to populate the map? *)
  let tempMap = ref Temp.empty

  let insertToTempMap map regs =
    List.fold_left
      ~f:(fun m (name, temp) -> Temp.enter (m, temp, name))
      ~init:map regs

  let processRegisterList regs =
    tempMap := insertToTempMap !tempMap regs;
    List.map ~f:snd regs

  let special_regs =
    processRegisterList
      [
        ("zero", Temp.new_temp ());
        ("sp", sp);
        ("s0", Temp.new_temp ()) (* Frame pointer *);
        ("ra", ra);
        ("a0", rv)
        (* TODO: should return value be here on in args register list? *);
        ("fp", fp);
      ]

  let arg_regs =
    processRegisterList
      [
        ("a1", Temp.new_temp ());
        ("a2", Temp.new_temp ());
        ("a3", Temp.new_temp ());
        ("a5", Temp.new_temp ());
        ("a6", Temp.new_temp ());
        ("a7", Temp.new_temp ());
      ]

  let callee_saves =
    processRegisterList
      [
        ("s1", Temp.new_temp ());
        ("s2", Temp.new_temp ());
        ("s3", Temp.new_temp ());
        ("s4", Temp.new_temp ());
        ("s5", Temp.new_temp ());
        ("s6", Temp.new_temp ());
        ("s7", Temp.new_temp ());
        ("s8", Temp.new_temp ());
        ("s9", Temp.new_temp ());
        ("s10", Temp.new_temp ());
        ("s11", Temp.new_temp ());
      ]

  let caller_saves =
    processRegisterList
      [
        ("t0", Temp.new_temp ());
        ("t1", Temp.new_temp ());
        ("t2", Temp.new_temp ());
        ("t3", Temp.new_temp ());
        ("t4", Temp.new_temp ());
        ("t5", Temp.new_temp ());
        ("t6", Temp.new_temp ());
      ]

  let registerToString temp =
    match Temp.look (!tempMap, temp) with
    | Some r -> r
    | None -> Temp.make_string temp

  let procEntryExit2 (_frame, body) =
    body
    @ [
        Assem.OPER
          {
            assem = "";
            (* To indicate to the register allocator that zero, return-address, stack-pointer
               registers are still live at the end of the function *)
            src = [ zero; ra; sp ] @ callee_saves;
            dst = [];
            jump = Some [];
          };
      ]

  type fn_prolog_epilog = {
    prolog : string;
    body : Assem.instr list;
    epilog : string;
  }

  let procEntryExit3 ((frame : frame), body) =
    {
      prolog =
        Printf.sprintf "\t.globl\t%s\n\t.type\t%s, @function\n"
          (Symbol.name frame.name) (Symbol.name frame.name);
      body;
      epilog = "";
    }

  let%test_unit "test_all_escape_formals" =
    let frame =
      new_frame
        { name = Temp.named_label "frame_name"; formals = [ true; true; true ] }
    in
    [%test_eq: access list] (formals frame) [ InFrame 0; InFrame 8; InFrame 16 ]

  let%test_unit "test_all_escape_locals" =
    let expected = [ InFrame (-24); InFrame (-32); InFrame (-40) ] in
    [%test_eq: access list]
      (List.map ~f:(fun i -> alloc_local true i) [ 0; 1; 2 ])
      expected

  let%test_unit "test_special_registers_named" =
    let expected = [ "zero"; "sp"; "s0"; "ra"; "a0"; "fp" ] in
    [%test_eq: register list]
      (List.map ~f:registerToString special_regs)
      expected

  let%test_unit "test_arg_registers_named" =
    let expected = [ "a1"; "a2"; "a3"; "a5"; "a6"; "a7" ] in
    [%test_eq: register list] (List.map ~f:registerToString arg_regs) expected

  let%test_unit "test_caller_saved_registers_named" =
    let expected = [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6" ] in
    [%test_eq: register list]
      (List.map ~f:registerToString caller_saves)
      expected

  let%test_unit "test_callee_saved_registers_named" =
    let expected =
      [ "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11" ]
    in
    [%test_eq: register list]
      (List.map ~f:registerToString callee_saves)
      expected
end

module Frame : FRAME = RiscVFrame
