open Errormsg
open Base
open Absyn
open Symbol

exception Parse_error

let parse lexbuf =
  let res =
    try [ Grammar.input Lexer.token lexbuf ]
    with Caml.Parsing.Parse_error ->
      ErrorMsg.error lexbuf "Syntax error";
      []
  in
  if !ErrorMsg.anyErrors then raise Parse_error else res

let parse_string s =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string s in
  parse lexbuf

let parse_file fname =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_channel (Stdio.In_channel.create fname) in
  Lexing.set_filename lexbuf fname;
  ErrorMsg.set_filename fname;
  parse lexbuf

open Base

let%expect_test "successfully_parse_test_files" =
  ignore
    (let test_dir = "../../../tests/" in
     Caml.Sys.readdir test_dir |> Array.to_list
     |> List.filter ~f:(fun x -> String.(Caml.Filename.extension x = ".tig"))
     |> List.map ~f:(fun fname -> parse_file (test_dir ^ fname)));
  [%expect]

let%test_unit "produce_ast_simple_binary_exp_with_ints" =
  [%test_eq: Absyn.exp list] (parse_string "1 + 2")
    [
      Absyn.OpExp
        {
          left = Absyn.IntExp 1;
          right = Absyn.IntExp 2;
          oper = Absyn.PlusOp;
          pos = 0;
        };
    ]

let%test_unit "produce_ast_simple_binary_exp_with_vars" =
  [%test_eq: Absyn.exp list] (parse_string "x - y")
    [
      Absyn.OpExp
        {
          left = Absyn.VarExp (Absyn.SimpleVar (Symbol.to_symbol "x", 0));
          right = Absyn.VarExp (Absyn.SimpleVar (Symbol.to_symbol "y", 4));
          oper = Absyn.MinusOp;
          pos = 0;
        };
    ]

let%test_unit "function_call_one_arg" =
  [%test_eq: Absyn.exp list]
    (parse_string "print(\"Hello world\")")
    [
      Absyn.CallExp
        {
          func = Symbol.to_symbol "print";
          args = [ Absyn.StringExp ("Hello world", 6) ];
          pos = 0;
        };
    ]

(* TODO: Write more tests for other productions *)
