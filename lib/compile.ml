open Semant

(* Runs parser on the string and performs
   semantic analysis on the resulting AST*)
let compile_string s =
  ignore (List.map Semant.transProg (Parser.parse_string s))

let compile_file filename =
  ignore (List.map Semant.transProg (Parser.parse_file filename))
