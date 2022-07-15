open Tokens
open Errormsg
open Base

let create_hashtable size init =
  let tbl = Hashtbl.create ~size (module String) in
  List.iter ~f:(fun (key, data) -> ignore (Hashtbl.add tbl ~key ~data)) init;
  tbl

let keyword_table =
  create_hashtable 8
    [
      ("type", Tokens._TYPE);
      ("var", Tokens._VAR);
      ("function", Tokens._FUNCTION);
      ("break", Tokens._BREAK);
      ("of", Tokens._OF);
      ("end", Tokens._END);
      ("in", Tokens._IN);
      ("let", Tokens._LET);
      ("do", Tokens._DO);
      ("to", Tokens._TO);
      ("for", Tokens._FOR);
      ("while", Tokens._WHILE);
      ("else", Tokens._ELSE);
      ("then", Tokens._THEN);
      ("if", Tokens._IF);
      ("array", Tokens._ARRAY);
      ("assign", Tokens._ASSIGN);
      ("or", Tokens._OR);
      ("and", Tokens._AND);
    ]

let __ocaml_lex_tables =
  {
    Lexing.lex_base =
      "\000\000\229\255\230\255\231\255\001\000\234\255\235\255\236\255\237\255\238\255\239\255\240\255\241\255\242\255\243\255\244\255\245\255\246\255\021\000\248\255\003\000\031\000\033\000\078\000\088\000\253\255\251\255\249\255\232\255\233\255\002\000\053\000\254\255\049\000\255\255";
    Lexing.lex_backtrk =
      "\255\255\255\255\255\255\255\255\025\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\008\000\255\255\025\000\005\000\003\000\001\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\255\255";
    Lexing.lex_default =
      "\002\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\030\000\032\000\000\000\255\255\000\000";
    Lexing.lex_trans =
      "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\020\000\004\000\029\000\029\000\000\000\000\000\000\000\009\000\008\000\017\000\015\000\005\000\016\000\014\000\018\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\007\000\006\000\021\000\019\000\022\000\028\000\027\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\011\000\026\000\010\000\025\000\033\000\034\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\013\000\000\000\012\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
    Lexing.lex_check =
      "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\004\000\030\000\255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\000\000\022\000\031\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\004\000\030\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\031\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255";
    Lexing.lex_base_code = "";
    Lexing.lex_backtrk_code = "";
    Lexing.lex_default_code = "";
    Lexing.lex_trans_code = "";
    Lexing.lex_check_code = "";
    Lexing.lex_code = "";
  }

let rec tiger_token lexbuf = __ocaml_lex_tiger_token_rec lexbuf 0

and __ocaml_lex_tiger_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
  | 0 ->
      let inum =
        Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos
          lexbuf.Lexing.lex_curr_pos
      in

      Tokens._INT
        ( Int.of_string inum,
          Lexing.lexeme_start lexbuf,
          Lexing.lexeme_end lexbuf )
  | 1 -> (
      let word =
        Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos
          lexbuf.Lexing.lex_curr_pos
      in

      match Hashtbl.find keyword_table word with
      | Some token_fn ->
          token_fn (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
      | None ->
          Tokens._ID (word, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
      )
  | 2 -> Tokens._GE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 3 -> Tokens._GT (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 4 -> Tokens._LE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 5 -> Tokens._LT (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 6 -> Tokens._NEQ (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 7 -> Tokens._EQ (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 8 -> Tokens._DIVIDE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 9 -> Tokens._TIMES (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 10 -> Tokens._MINUS (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 11 -> Tokens._PLUS (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 12 -> Tokens._DOT (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 13 -> Tokens._LBRACE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 14 -> Tokens._RBRACE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 15 -> Tokens._LBRACK (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 16 -> Tokens._RBRACK (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 17 -> Tokens._LPAREN (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 18 -> Tokens._RPAREN (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 19 -> Tokens._COLON (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 20 ->
      Tokens._SEMICOLON (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 21 -> Tokens._COMMA (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 22 ->
      let s =
        Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos
          lexbuf.Lexing.lex_curr_pos
      in

      Tokens._STRING
        ( String.sub s ~pos:1 ~len:(String.length s - 2),
          Lexing.lexeme_start lexbuf,
          Lexing.lexeme_end lexbuf )
  | 23 -> comment lexbuf
  | 24 -> tiger_token lexbuf
  | 25 ->
      ErrorMsg.error (Lexing.lexeme_start lexbuf) "Invalid token";
      Tokens._EOF (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | 26 -> Tokens._EOF (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
  | __ocaml_lex_state ->
      lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_tiger_token_rec lexbuf __ocaml_lex_state

and comment lexbuf = __ocaml_lex_comment_rec lexbuf 31

and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
  | 0 -> tiger_token lexbuf
  | 1 -> comment lexbuf
  | __ocaml_lex_state ->
      lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

let parse lexbuf =
  let rec parse' lexbuf acc =
    let curr_token = tiger_token lexbuf in
    let res = curr_token :: acc in
    if Tokens.is_eof curr_token then res else parse' lexbuf res
  in
  List.rev (parse' lexbuf [])

let%test_unit "parse_single_int" =
  let lexbuf = Lexing.from_string "42" in
  [%test_eq: Tokens.token list] (parse lexbuf)
    [ Tokens._INT (42, 0, 1); Tokens._EOF (2, 2) ]

let%test_unit "parse_random_keywords" =
  let lexbuf = Lexing.from_string "var type break" in
  [%test_eq: Tokens.token list] (parse lexbuf)
    [
      Tokens._VAR (0, 0);
      Tokens._TYPE (0, 0);
      Tokens._BREAK (0, 0);
      Tokens._EOF (0, 0);
    ]

let%test_unit "parse_test1.tig" =
  let lexbuf =
    Lexing.from_string
      "/* an array type and an array variable */ let type arrtype = array of \
       int var arr1:arrtype := arrtype [10] of 0 in arr1 end "
  in
  [%test_eq: Tokens.token list] (parse lexbuf)
    [
      Tokens._LET (0, 0);
      Tokens._TYPE (0, 0);
      Tokens._ID ("arrtype", 0, 0);
      Tokens._EQ (0, 0);
      Tokens._ARRAY (0, 0);
      Tokens._OF (0, 0);
      Tokens._ID ("int", 0, 0);
      Tokens._VAR (0, 0);
      Tokens._ID ("arr1", 0, 0);
      Tokens._COLON (0, 0);
      Tokens._ID ("arrtype", 0, 0);
      Tokens._COLON (0, 0);
      Tokens._EQ (0, 0);
      Tokens._ID ("arrtype", 0, 0);
      Tokens._LBRACK (0, 0);
      Tokens._INT (10, 0, 0);
      Tokens._RBRACK (0, 0);
      Tokens._OF (0, 0);
      Tokens._INT (0, 0, 0);
      Tokens._IN (0, 0);
      Tokens._ID ("arr1", 0, 0);
      Tokens._END (0, 0);
      Tokens._EOF (0, 0);
    ]

let%test_unit "parse_string" =
  let lexbuf = Lexing.from_string "\"A string\"" in
  [%test_eq: Tokens.token list] (parse lexbuf)
    [ Tokens._STRING ("A string", 0, 1); Tokens._EOF (2, 2) ]
