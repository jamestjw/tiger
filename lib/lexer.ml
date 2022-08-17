open Errormsg
open Base

let create_hashtable size init =
  let tbl = Hashtbl.create ~size (module String) in
  List.iter ~f:(fun (key, data) -> ignore (Hashtbl.add tbl ~key ~data)) init;
  tbl

let keyword_table =
  create_hashtable 8
    [
      ("type", Grammar.TYPE);
      ("var", Grammar.VAR);
      ("function", Grammar.FUNCTION);
      ("break", Grammar.BREAK);
      ("of", Grammar.OF);
      ("end", Grammar.END);
      ("in", Grammar.IN);
      ("let", Grammar.LET);
      ("do", Grammar.DO);
      ("to", Grammar.TO);
      ("for", Grammar.FOR);
      ("while", Grammar.WHILE);
      ("else", Grammar.ELSE);
      ("then", Grammar.THEN);
      ("if", Grammar.IF);
      ("array", Grammar.ARRAY);
      ("assign", Grammar.ASSIGN);
      ("or", Grammar.OR);
      ("and", Grammar.AND);
      ("nil", Grammar.NIL);
    ]

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    {
      pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      (* Increment line number *)
      Lexing.pos_bol = pos.Lexing.pos_cnum (* Increment beginning of line *);
    };
  ErrorMsg.line_num := !ErrorMsg.line_num + 1;
  ErrorMsg.line_pos := pos.Lexing.pos_cnum :: !ErrorMsg.line_pos

let __ocaml_lex_tables =
  {
    Lexing.lex_base =
      "\000\000\225\255\226\255\227\255\228\255\001\000\231\255\232\255\233\255\234\255\236\255\237\255\238\255\239\255\240\255\241\255\242\255\243\255\244\255\245\255\021\000\247\255\033\000\003\000\031\000\078\000\153\000\253\255\252\255\248\255\250\255\229\255\230\255\002\000\127\000\253\255\254\255\049\000\255\255";
    Lexing.lex_backtrk =
      "\255\255\255\255\255\255\255\255\255\255\029\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\009\000\255\255\006\000\004\000\020\000\001\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\002\000\255\255";
    Lexing.lex_default =
      "\002\000\000\000\000\000\000\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\033\000\035\000\000\000\000\000\255\255\000\000";
    Lexing.lex_trans =
      "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\003\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\005\000\032\000\032\000\000\000\007\000\000\000\011\000\010\000\019\000\017\000\008\000\018\000\016\000\020\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\024\000\009\000\022\000\021\000\023\000\031\000\028\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\013\000\027\000\012\000\030\000\029\000\038\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\015\000\006\000\014\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\000\000\036\000\000\000\000\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\037\000\000\000\000\000\000\000\025\000\000\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
    Lexing.lex_check =
      "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\000\000\005\000\033\000\255\255\000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\000\000\022\000\022\000\037\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\255\255\034\000\255\255\255\255\255\255\255\255\255\255\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\034\000\255\255\255\255\255\255\025\000\255\255\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\005\000\033\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\034\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255";
    Lexing.lex_base_code = "";
    Lexing.lex_backtrk_code = "";
    Lexing.lex_default_code = "";
    Lexing.lex_trans_code = "";
    Lexing.lex_check_code = "";
    Lexing.lex_code = "";
  }

let rec token lexbuf = __ocaml_lex_token_rec lexbuf 0

and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
  | 0 ->
      let inum =
        Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos
          lexbuf.Lexing.lex_curr_pos
      in
      Grammar.INT (Int.of_string inum)
  | 1 -> (
      let word =
        Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos
          lexbuf.Lexing.lex_curr_pos
      in
      match Hashtbl.find keyword_table word with
      | Some tok -> tok
      | None -> Grammar.ID word)
  | 2 -> Grammar.ASSIGN
  | 3 -> Grammar.GE
  | 4 -> Grammar.GT
  | 5 -> Grammar.LE
  | 6 -> Grammar.LT
  | 7 -> Grammar.NEQ
  | 8 -> Grammar.EQ
  | 9 -> Grammar.DIVIDE
  | 10 -> Grammar.TIMES
  | 11 -> Grammar.MINUS
  | 12 -> Grammar.PLUS
  | 13 -> Grammar.DOT
  | 14 -> Grammar.LBRACE
  | 15 -> Grammar.RBRACE
  | 16 -> Grammar.LBRACK
  | 17 -> Grammar.RBRACK
  | 18 -> Grammar.LPAREN
  | 19 -> Grammar.RPAREN
  | 20 -> Grammar.COLON
  | 21 -> Grammar.SEMICOLON
  | 22 -> Grammar.COMMA
  | 23 -> Grammar.AND
  | 24 -> Grammar.OR
  | 25 ->
      let s =
        Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos
          lexbuf.Lexing.lex_curr_pos
      in
      Grammar.STRING (String.sub s ~pos:1 ~len:(String.length s - 2))
  | 26 -> comment lexbuf
  | 27 -> token lexbuf
  | 28 ->
      incr_linenum lexbuf;
      token lexbuf
  | 29 ->
      ErrorMsg.error lexbuf "Invalid token";
      token lexbuf
  | 30 -> Grammar.EOF
  | __ocaml_lex_state ->
      lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf = __ocaml_lex_comment_rec lexbuf 34

and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
  | 0 -> token lexbuf
  | 1 ->
      incr_linenum lexbuf;
      comment lexbuf
  | 2 -> comment lexbuf
  | __ocaml_lex_state ->
      lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

let parse lexbuf =
  let rec parse' lexbuf acc =
    let curr_token = token lexbuf in
    if Poly.(curr_token = Grammar.EOF) then curr_token :: acc
    else parse' lexbuf (curr_token :: acc)
  in
  let res = List.rev (parse' lexbuf []) in
  if !ErrorMsg.anyErrors then raise ErrorMsg.Error else res

(* Unit tests *)
let%test_unit "parse_single_int" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "42" in
  [%test_eq: Grammar.token list] (parse lexbuf) [ Grammar.INT 42; Grammar.EOF ]

let%test_unit "parse_random_keywords" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "var type break" in
  [%test_eq: Grammar.token list] (parse lexbuf)
    [ Grammar.VAR; Grammar.TYPE; Grammar.BREAK; Grammar.EOF ]

let%test_unit "parse_test1.tig" =
  ErrorMsg.reset ();
  let lexbuf =
    Lexing.from_string
      "/* an array type and an array variable */ let type arrtype = array of \
       int var arr1:arrtype := arrtype [10] of 0 in arr1 end "
  in
  [%test_eq: Grammar.token list] (parse lexbuf)
    [
      Grammar.LET;
      Grammar.TYPE;
      Grammar.ID "arrtype";
      Grammar.EQ;
      Grammar.ARRAY;
      Grammar.OF;
      Grammar.ID "int";
      Grammar.VAR;
      Grammar.ID "arr1";
      Grammar.COLON;
      Grammar.ID "arrtype";
      Grammar.ASSIGN;
      Grammar.ID "arrtype";
      Grammar.LBRACK;
      Grammar.INT 10;
      Grammar.RBRACK;
      Grammar.OF;
      Grammar.INT 0;
      Grammar.IN;
      Grammar.ID "arr1";
      Grammar.END;
      Grammar.EOF;
    ]

let%test_unit "parse_string" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "\"A string\"" in
  [%test_eq: Grammar.token list] (parse lexbuf)
    [ Grammar.STRING "A string"; Grammar.EOF ]

exception Missing_exception

let%expect_test "parse_invalid_symbol" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "let type arrtype = ~array of" in
  try
    ignore (parse lexbuf);
    raise Missing_exception
  with ErrorMsg.Error ->
    ();
    [%expect {| :1.20: Invalid token |}]

let%expect_test "parse_invalid_symbol_line2" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "let type arrtype = array of\ncat ` hello" in
  try
    ignore (parse lexbuf);
    raise Missing_exception
  with ErrorMsg.Error ->
    ();
    [%expect {| :2.5: Invalid token |}]

let%expect_test "parse_invalid_symbol_line3" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "let type\ncat hello\nwrong $ token" in
  try
    ignore (parse lexbuf);
    raise Missing_exception
  with ErrorMsg.Error ->
    ();
    [%expect {| :3.7: Invalid token |}]

let%expect_test "successfully_parse_test_files" =
  ErrorMsg.reset ();
  ignore
    (let test_dir = "../../../tests/" in
     Caml.Sys.readdir test_dir |> Array.to_list
     |> List.filter ~f:(fun x -> String.(Caml.Filename.extension x = ".tig"))
     |> List.map ~f:(fun fname ->
            parse
              (Lexing.from_channel (Stdio.In_channel.create (test_dir ^ fname)))));
  [%expect]
