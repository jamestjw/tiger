open Errormsg
open Base
open Absyn
open Symbol
module A = Absyn

exception Parse_error

let parse lexbuf =
  let res =
    try Grammar.input Lexer.token lexbuf
    with Stdlib.Parsing.Parse_error ->
      ErrorMsg.error lexbuf "Syntax error";
      raise Parse_error
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
     Stdlib.Sys.readdir test_dir
     |> Array.to_list
     |> List.filter ~f:(fun x -> String.(Stdlib.Filename.extension x = ".tig"))
     |> List.map ~f:(fun fname -> parse_file (test_dir ^ fname)));
  [%expect]

let%test_unit "produce_ast_simple_binary_exp_with_ints" =
  [%test_eq: A.exp] (parse_string "1 + 2")
    (A.OpExp { left = A.IntExp 1; right = A.IntExp 2; oper = A.PlusOp; pos = 0 })

let%test_unit "produce_ast_simple_binary_exp_with_vars" =
  [%test_eq: A.exp] (parse_string "x - y")
    (A.OpExp
       {
         left = A.VarExp (A.SimpleVar (Symbol.to_symbol "x", 0));
         right = A.VarExp (A.SimpleVar (Symbol.to_symbol "y", 4));
         oper = A.MinusOp;
         pos = 0;
       })

let%test_unit "function_call_one_arg" =
  [%test_eq: A.exp]
    (parse_string "print(\"Hello world\")")
    (A.CallExp
       {
         func = Symbol.to_symbol "print";
         args = [ A.StringExp ("Hello world", 6) ];
         pos = 0;
       })

let%test_unit "let_expr" =
  [%test_eq: A.exp]
    (parse_string
       {|
      let
        var N := 8
        function getN(): int = N
        in
        getN()
      end 
    |})
    (A.LetExp
       {
         decs =
           [
             A.VarDec
               {
                 name = Symbol.to_symbol "N";
                 escape = ref true;
                 typ = None;
                 init = A.IntExp 8;
                 pos = 19;
               };
             A.FunctionDec
               {
                 name = Symbol.to_symbol "getN";
                 params = [];
                 result = Some (Symbol.to_symbol "int", 55);
                 body = A.VarExp (A.SimpleVar (Symbol.to_symbol "N", 61));
                 pos = 38;
               };
           ];
         pos = 7;
         body =
           A.SeqExp
             [
               ( A.CallExp
                   { func = Symbol.to_symbol "getN"; args = []; pos = 82 },
                 82 );
             ];
       })

(* TODO: Write more tests for other productions *)
