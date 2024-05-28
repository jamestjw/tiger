open Tiger
open Findescape
open Semant
open Canon
open Codegen
open Base
open Assem
open Frame
open Regalloc

let input_file = ref ""
let output_file = ref ""
let usage_msg = "tiger -o output-file input-file"

let speclist =
  [ ("-o", Stdlib.Arg.Set_string output_file, "Set output file name") ]

let add_input_file fn = input_file := fn

let generateFunctionStm stm frame =
  let stm = Canon.canonize stm in
  let instrs =
    Frame.procEntryExit2
      (frame, Stdlib.List.flatten (List.map ~f:(RiscVGen.codegen frame) stm))
  in
  let ({ body; _ } as fn_prolog_epilog : Frame.fn_prolog_epilog) =
    Frame.procEntryExit3 (frame, instrs)
  in
  (* let  allocation = Frame.get_temp_map () in *)
  let body, allocation = RegAlloc.alloc body frame in
  Frame.fn_prolog_epilog_to_string
    { fn_prolog_epilog with body }
    ~register_map:allocation

let generateFrag = function
  | Frame.PROC { body; frame } -> generateFunctionStm body frame
  | Frame.STRING (lab, str) ->
      Assem.format Frame.register_to_string_default
        (RiscVGen.generateString lab str)

(* TODO: Use a more reliable library to handle this *)
let main () =
  Stdlib.Arg.parse speclist add_input_file usage_msg;
  if String.(!input_file = "") then (
    Stdio.print_string "Error: Missing input file\n";
    Stdlib.exit 1);

  let outc =
    if String.(!output_file = "") then Stdio.Out_channel.stdout
    else
      Stdio.Out_channel.create !output_file ~append:false ~fail_if_exists:false
  in

  let absyn = Parser.parse_file !input_file in
  let frags =
    FindEscape.find_escape absyn;
    Semant.transProg absyn
  in

  let instrs = List.map frags ~f:generateFrag in

  (* Overwrite and truncate *)
  List.iter ~f:(fun instr -> Stdlib.Printf.fprintf outc "%s" instr) instrs;
  Stdio.Out_channel.close outc

let () = main ()
