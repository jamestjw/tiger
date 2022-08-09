open Errormsg

exception Parse_error

let parse lexbuf =
  ErrorMsg.reset ();
  let rec parse' acc =
    try parse' (acc @ [ Grammar.input Lexer.token lexbuf ]) with
    | End_of_file -> acc
    | Parsing.Parse_error ->
        ErrorMsg.error lexbuf "Syntax error";
        []
  in
  let res = parse' [] in
  if !ErrorMsg.anyErrors then raise Parse_error else res

let parse_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf

let parse_file fname =
  let lexbuf = Lexing.from_channel (Stdio.In_channel.create fname) in
  Lexing.set_filename lexbuf fname;
  parse lexbuf

open Base

let%expect_test "successfully_parse_test_files" =
  ignore
    (let test_dir = "../../../tests/" in
     Caml.Sys.readdir test_dir |> Array.to_list
     |> List.filter ~f:(fun x -> String.(Caml.Filename.extension x = ".tig"))
     |> List.map ~f:(fun fname -> parse_file (test_dir ^ fname)));
  [%expect]

(* TODO: Write tests for AST production *)
(* let%test_unit "produce_ast_simple_binary_exp" =
   [%test_eq: Absyn.Absyn.exp list] (parse_string "x + y") [] *)
