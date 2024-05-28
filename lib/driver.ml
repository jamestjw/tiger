open Findescape
open Semant
open Canon
open Codegen
open Base
open Frame
open Regalloc

let generateFrag f =
  let generateFunctionStm stm frame =
    let stm = Canon.canonize stm in
    let instrs =
      Frame.procEntryExit2
        (frame, Stdlib.List.flatten (List.map ~f:(RiscVGen.codegen frame) stm))
    in
    let ({ body; _ } as fn_prolog_epilog : Frame.fn_prolog_epilog) =
      Frame.procEntryExit3 (frame, instrs)
    in
    let body, allocation = RegAlloc.alloc body frame in
    Frame.fn_prolog_epilog_to_string
      { fn_prolog_epilog with body }
      ~register_map:allocation
  in
  match f with
  | Frame.PROC { body; frame } -> generateFunctionStm body frame
  | Frame.STRING (lab, str) -> Frame.string lab str

let compile_file ~filename =
  let absyn = Parser.parse_file filename in
  let frags =
    FindEscape.find_escape absyn;
    Semant.transProg absyn
  in

  List.map frags ~f:generateFrag |> String.concat ~sep:""
