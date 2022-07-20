open Errormsg

let parse fname =
  let lexbuf = Lexing.from_channel (Stdio.In_channel.create fname) in
  Lexing.set_filename lexbuf fname;
  try
    while true do
      Grammar.input Lexer.token lexbuf
    done
  with
  | End_of_file -> ()
  | Parsing.Parse_error -> ErrorMsg.error lexbuf "Syntax error"

open Base

let%expect_test "successfully_parse_test_files" =
  ErrorMsg.reset ();
  ignore
    (let test_dir = "../../../../tests/" in
     Caml.Sys.readdir test_dir |> Array.to_list
     |> List.filter ~f:(fun x -> String.(Caml.Filename.extension x = ".tig"))
     |> List.map ~f:(fun fname -> parse (test_dir ^ fname)));
  [%expect]
