open Assem
open Base
open Symbol
open Temp
open Tree

module type FRAME = sig
  type frame [@@deriving show]

  type access = InFrame of int | InReg of Temp.temp
  [@@deriving ord, sexp, show]

  type new_frame_args = { name : Temp.label; formals : bool list }

  (* Fragments that are processed at the beginning of code generation,
     usually for statics and function definitions *)
  type frag =
    | PROC of { body : Tree.stm; frame : frame }
    | STRING of Temp.label * string
    | STRING_LIT of Temp.label * string

  val new_frame : new_frame_args -> frame
  val name : frame -> Temp.label
  val debug_frame : frame -> string

  (* Extracts a list of k "accesses" denoting the locations
     where the formal parameters will be kept at run time,
     as seen from inside the callee *)
  val formals : frame -> access list
  val alloc_local : frame -> bool -> access
  val num_locals : frame -> int

  (* The expression that is passed in is the address of the stack frame
     that the access lives in*)
  val exp : access -> Tree.exp -> Tree.exp
  val word_size : int

  (* Special registers:
     1. Frame pointer
     2. Return value
     3. Return address
     4. Stack pointer
     5. zero *)
  val fp : Temp.temp
  val rv : Temp.temp
  val ra : Temp.temp
  val sp : Temp.temp
  val zero : Temp.temp
  val externalCall : string * Tree.exp list -> Tree.exp

  type register [@@deriving ord]

  val special_regs : Temp.temp list
  val arg_regs : Temp.temp list
  val callee_saves : Temp.temp list
  val caller_saves : Temp.temp list
  val register_to_string : register Temp.tbl -> Temp.temp -> string
  val register_to_string_default : Temp.temp -> string
  val register_eq : register -> register -> bool
  val get_temp_map : unit -> register Temp.tbl
  val dummy_register_list : int -> register list

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

  val fn_prolog_epilog_to_string :
    fn_prolog_epilog -> register_map:register Temp.tbl -> string

  val string_obj : Symbol.symbol -> string -> string
  val string_lit : Symbol.symbol -> string -> string
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
  type access = InFrame of int | InReg of Temp.temp
  [@@deriving ord, sexp, show]

  type frame = {
    name : Temp.label;
    (* Ways to access the formal, based on whether or not it escapes *)
    formals : access list;
    num_locals : int ref;
    formal_moves : Tree.stm list;
    (* Maximum number of parameters involved in a call, this is populated
       by `procEntryExit1`.
       NOTE: The book says that it should be done by `procEntryExit2`,
       but I think it was a mistake as this function only has access to
       the instruction list, i.e. it can't easily tell where the parameters
       are. *)
    max_num_parameters : int ref;
  }
  [@@deriving show]

  type new_frame_args = { name : Temp.label; formals : bool list }

  type frag =
    | PROC of { body : Tree.stm; frame : frame }
    | STRING of Temp.label * string
    | STRING_LIT of Temp.label * string

  let word_size = 8

  (* First available position in the stack seems to be -16(fp). We are only
     storing the caller's FP here. Though we conservatively use 16 bytes
     instead of 8 because of:
     https://github.com/riscvarchive/riscv-gcc/issues/61 *)
  let local_start_offset = -16
  let name ({ name; _ } : frame) = name

  let debug_frame { name; formals; num_locals; _ } =
    Printf.sprintf "Name : %s, Num formals: %d, Num locals: %d"
      (Symbol.name name) (List.length formals) !num_locals

  let formals ({ formals; _ } : frame) = formals

  let alloc_local ({ num_locals; _ } as frame) is_escape =
    let num_locals = !num_locals in
    frame.num_locals := num_locals + 1;
    if is_escape then InFrame (local_start_offset - (word_size * num_locals))
    else InReg (Temp.new_temp ())

  let num_locals { num_locals; _ } = !num_locals

  (* Base of the stack frame *)
  let fp = Temp.new_temp ()
  let rv = Temp.new_temp ()
  let ra = Temp.new_temp ()

  (* Next available memory location on the stack *)
  let sp = Temp.new_temp ()
  let zero = Temp.new_temp ()

  let exp a e =
    match a with
    | InFrame k -> Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST k))
    | InReg r -> Tree.TEMP r

  let externalCall (s, args) = Tree.CALL (Tree.NAME (Temp.named_label s), args)

  type register = string [@@deriving ord, sexp]

  let register_eq : string -> string -> bool = String.equal

  (* TODO: Is this the only way to populate the map? *)
  let tempMap : register Temp.tbl ref = ref Temp.empty

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
        ("zero", zero);
        (* Stack pointer *)
        ("sp", sp);
        (* Return address *)
        ("ra", ra);
        (* Frame pointer, also knowned in the ABI as `s0` *)
        ("fp", fp);
      ]

  let arg_regs =
    processRegisterList
      [
        (* Return value *)
        ("a0", rv);
        ("a1", Temp.new_temp ());
        ("a2", Temp.new_temp ());
        ("a3", Temp.new_temp ());
        ("a4", Temp.new_temp ());
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

  let get_temp_map () = !tempMap

  let register_to_string map temp =
    match Temp.look (map, temp) with
    | Some r -> r
    | None -> Temp.make_string temp

  let register_to_string_default = register_to_string @@ get_temp_map ()

  let new_frame { name; formals } =
    let _, _, num_locals, formals, formal_moves =
      List.fold_left
        ~f:(fun (i, num_reg, num_stack, l, moves) escape ->
          let src =
            match List.nth arg_regs i with
            | Some reg ->
                (* We would have put it in the i-th arg register if
                   it was possible *)
                Tree.TEMP reg
            | None ->
                (* Otherwise, we would have put it on the stack in reverse order *)
                Tree.MEM
                  (Tree.BINOP
                     ( Tree.PLUS,
                       Tree.TEMP fp,
                       Tree.CONST ((i - List.length arg_regs) * word_size) ))
          in

          if not escape then
            let dest_temp = Temp.new_temp () in
            let move = Tree.MOVE (Tree.TEMP dest_temp, src) in
            (i + 1, num_reg + 1, num_stack, InReg dest_temp :: l, move :: moves)
          else
            let offset = local_start_offset - (word_size * num_stack) in
            let move =
              Tree.MOVE
                ( Tree.MEM
                    (Tree.BINOP (Tree.PLUS, Tree.CONST offset, Tree.TEMP fp)),
                  src )
            in
            (i + 1, num_reg, num_stack + 1, InFrame offset :: l, move :: moves))
        ~init:(0, 0, 0, [], []) formals
    in
    {
      name;
      formals = List.rev formals;
      num_locals = ref num_locals;
      max_num_parameters = ref 0;
      formal_moves;
    }

  let procEntryExit1 ({ formal_moves; max_num_parameters; _ }, stm) =
    (* Take this opportunity to calculate what is the max number of
       params involved in a call so we can adjust the SP later. *)
    max_num_parameters := Tree.max_call_param_count stm;

    (* Store these registers in temporaries, register allocation will
       spill them if necessary (i.e. if they are trashed by this function).
       The ones that are not spilled will be coalesced, i.e. the moves will
       be removed. *)
    let stm =
      List.fold (ra :: callee_saves) ~init:stm ~f:(fun stm t ->
          let temp = Temp.new_temp () in
          Tree.SEQ
            ( Tree.SEQ (Tree.MOVE (Tree.TEMP temp, Tree.TEMP t), stm),
              Tree.MOVE (Tree.TEMP t, Tree.TEMP temp) ))
    in
    (* Move formals to where the function expects them to be, i.e. from the
       registers or the stack to some other registers or the function's stack
       frame.*)
    List.fold formal_moves ~init:stm ~f:(fun prev stm -> Tree.SEQ (stm, prev))

  let procEntryExit2 (_frame, body) =
    body
    @ [
        Assem.OPER
          {
            assem = "";
            (* To indicate to the register allocator that zero,
               return-address, stack-pointer registers are still live at
               the end of the function. If they are live at the end,
               then they are live throughout, making it impossible for
               the register allocator to do anything with them. *)
            (* src = [ zero; ra; sp ] @ callee_saves; *)
            src = special_regs @ callee_saves;
            dst = [];
            jump = Some [];
          };
      ]

  type fn_prolog_epilog = {
    prolog : string;
    body : Assem.instr list;
    epilog : string;
  }

  let fn_prolog_epilog_to_string { prolog; body; epilog } ~register_map =
    let body =
      List.fold_left
        ~f:(fun acc instr ->
          acc ^ Assem.format (register_to_string register_map) instr)
        ~init:"" body
    in
    (* Body always ends with newline, so we should be able to skip it *)
    Printf.sprintf "%s\n%s%s\n" prolog body epilog

  let procEntryExit3
      (({ name; num_locals; max_num_parameters; _ } : frame), body) =
    let name = Symbol.name name in
    let max_params_on_stack =
      Int.max 0 (!max_num_parameters - List.length arg_regs)
    in
    let num_locals = !num_locals + max_params_on_stack in
    (* TODO: Verify that we want this -1 *)
    (* TODO: SP should always be 16-byte aligned according to
       https://en.wikichip.org/wiki/risc-v/registers*)
    let sp_offset = local_start_offset - (word_size * (num_locals - 1)) in
    let fn_header =
      Printf.sprintf "\t.text\n\t.globl\t%s\n\t.type\t%s, @function\n%s:\n" name
        name name
    in
    let stack_allocation = Printf.sprintf "\taddi sp, sp, %d\n" sp_offset in
    let save_caller_fp =
      Printf.sprintf "\tsd fp, %d(sp)\n" (-sp_offset - word_size)
    in
    let set_fp = Printf.sprintf "\taddi fp, sp, %d\n" (-sp_offset) in
    let prolog =
      List.fold
        [ fn_header; stack_allocation; save_caller_fp; set_fp ]
        ~init:"" ~f:( ^ )
    in

    let restore_sp = "\tmv sp, fp\n" in
    let restore_caller_fp = Printf.sprintf "\tld fp, %d(fp)\n" (-word_size) in
    let jump_to_return_address = "\tjr ra\n" in

    let epilog =
      List.fold
        [ restore_sp; restore_caller_fp; jump_to_return_address ]
        ~init:"" ~f:( ^ )
    in
    { prolog; body; epilog }

  (* Instruction to generate a string literal with a label. *)
  let string_lit label str =
    Printf.sprintf "\t.section\t.rodata\n\t.align\t3\n%s:\n\t.string\t\"%s\"\n"
      (Symbol.name label) str

  (* Instruction to generate a string object with a label. *)
  let string_obj label str =
    let label = Symbol.name label in
    let tag_label = Temp.new_label () in
    let char_list = String.to_list @@ Stdlib.Scanf.unescaped str in
    let tag_code = string_lit tag_label "!s" in
    let strlen = List.length char_list in
    (* We are aligning this to 8, i.e. .align 3 (power of 2) *)
    let padding = 8 - (strlen % 8) in
    (* +8 because the first 8 bytes is used for the pointer to the tag *)
    (* +8 because the next 8 bytes is used for the string length *)
    (* `!s` is the tag that the runtime uses for strings literals, i.e.
       don't need to be GC-ed. For convenience, we keep emitting the tag
       literal, though this should be optimised for sure. *)
    let preamble =
      Printf.sprintf
        "\t.globl\t%s\n\
         %s\t.data\n\
         \t.align\t3\n\
         \t.type\t%s, @object\n\
         \t.size\t%s, %d\n\
         %s:\n\
         \t.dword\t%s\n\
         \t.dword\t%d\n"
        label tag_code label label
        (8 + 8 + strlen + padding)
        label (Symbol.name tag_label) strlen
    in
    let body =
      List.map char_list ~f:(fun c ->
          Printf.sprintf "\t.byte\t%d" @@ Stdlib.Char.code c)
      |> String.concat ~sep:"\n"
    in
    let postamble =
      if padding > 0 then Printf.sprintf "\n\t.zero\t%d\n" padding else "\n"
    in
    preamble ^ body ^ postamble

  let dummy_register_list i = List.init i ~f:(fun i -> Printf.sprintf "r%d" i)

  (* Start of tests *)

  let%test_unit "test_all_escape_formals" =
    let frame =
      new_frame
        { name = Temp.named_label "frame_name"; formals = [ true; true; true ] }
    in
    [%test_eq: access list] (formals frame)
      [ InFrame (-16); InFrame (-24); InFrame (-32) ]

  let%test_unit "test_all_escape_locals" =
    let expected = [ InFrame (-16); InFrame (-24); InFrame (-32) ] in
    let frame = new_frame { name = Temp.named_label "test"; formals = [] } in
    [%test_eq: access list]
      (List.map ~f:(fun i -> alloc_local frame true) [ 0; 1; 2 ])
      expected

  let%test_unit "test_special_registers_named" =
    let expected = [ "zero"; "sp"; "ra"; "fp" ] in
    [%test_eq: register list]
      (List.map ~f:register_to_string_default special_regs)
      expected

  let%test_unit "test_arg_registers_named" =
    let expected = [ "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7" ] in
    [%test_eq: register list]
      (List.map ~f:register_to_string_default arg_regs)
      expected

  let%test_unit "test_caller_saved_registers_named" =
    let expected = [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6" ] in
    [%test_eq: register list]
      (List.map ~f:register_to_string_default caller_saves)
      expected

  let%test_unit "test_callee_saved_registers_named" =
    let expected =
      [ "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11" ]
    in
    [%test_eq: register list]
      (List.map ~f:register_to_string_default callee_saves)
      expected
end

module Frame : FRAME = RiscVFrame
