open Tiger
open Findescape
open Semant
open Canon
open Translate
open Codegen
open Base
open Assem
open Frame

let input_file = ref ""
let output_file = ref ""
let usage_msg = "tiger -o output-file input-file"

let speclist =
  [ ("-o", Caml.Arg.Set_string output_file, "Set output file name") ]

let add_input_file fn = input_file := fn

let canonize stm =
  Canon.linearize stm |> Canon.basicBlocks |> Canon.traceSchedule

let generateFunctionStm stm frame =
  let stm' = canonize stm in
  let instrs =
    Frame.procEntryExit2
      (frame, Caml.List.flatten (List.map ~f:(RiscVGen.codegen frame) stm'))
  in
  let data = Frame.procEntryExit3 (frame, instrs) in
  Printf.sprintf "%s\n%s\n%s\n" data.prolog
    (List.fold_left
       ~f:(fun acc instr -> acc ^ Assem.format Frame.registerToString instr)
       ~init:"" data.body)
    data.epilog

let generateFrag = function
  | Frame.PROC { body; frame } -> generateFunctionStm body frame
  | Frame.STRING (lab, str) ->
      Assem.format Frame.registerToString (RiscVGen.generateString lab str)

(* TODO: Use a more reliable library to handle this *)
let main () =
  Caml.Arg.parse speclist add_input_file usage_msg;
  if String.(!input_file = "") then (
    Stdio.print_string "Error: Missing input file\n";
    Caml.exit 1);
  if String.(!output_file = "") then (
    Stdio.print_string "Error: Missing output file\n";
    Caml.exit 1);

  let absyn = Parser.parse_file !input_file in
  let exp, frags =
    FindEscape.find_escape absyn;
    Semant.transProg absyn
  in
  let stm = Translate.unNx exp in

  let instrs =
    List.map frags ~f:(fun frag -> generateFrag frag)
    @ [ generateFunctionStm stm Translate.outermost.frame ]
  in
  (* Overwrite and truncate *)
  let outc =
    Stdio.Out_channel.create !output_file ~append:false ~fail_if_exists:false
  in
  List.iter ~f:(fun instr -> Caml.Printf.fprintf outc "%s" instr) instrs;
  Stdio.Out_channel.close outc

let () = main ()
