open Semant
open Findescape

(* Steps:
   1. Runs parser on the string to build AST
   2. Run variable escape analaysis on the AST
   3. Run emantic analysis on the AST*)

let process_ast ast =
  FindEscape.find_escape ast;
  Semant.transProg ast

let compile_string s = process_ast (Parser.parse_string s)
let compile_file filename = process_ast (Parser.parse_file filename)
