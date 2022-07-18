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
    ]

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    {
      pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      (* Increment line number *)
      Lexing.pos_bol = pos.Lexing.pos_cnum (* Increment beginning of line *);
    }

let __ocaml_lex_tables =
  {
    Lexing.lex_base =
      "\000\000\225\255\226\255\227\255\228\255\001\000\231\255\232\255\233\255\234\255\002\000\237\255\238\255\239\255\240\255\241\255\242\255\243\255\244\255\245\255\246\255\022\000\248\255\033\000\031\000\078\000\153\000\253\255\249\255\251\255\229\255\236\255\230\255\002\000\054\000\254\255\089\000\255\255";
    Lexing.lex_backtrk =
      "\255\255\255\255\255\255\255\255\255\255\029\000\255\255\255\255\255\255\255\255\020\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\008\000\255\255\005\000\003\000\001\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\255\255";
    Lexing.lex_default =
      "\002\000\000\000\000\000\000\000\000\000\033\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\033\000\035\000\000\000\255\255\000\000";
    Lexing.lex_trans =
      "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\003\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\005\000\032\000\032\000\000\000\007\000\000\000\012\000\011\000\020\000\018\000\008\000\019\000\017\000\021\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\010\000\009\000\023\000\022\000\024\000\031\000\030\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\014\000\027\000\013\000\029\000\028\000\036\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\016\000\006\000\015\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\037\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
    Lexing.lex_check =
      "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\000\000\005\000\033\000\255\255\000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\000\000\023\000\023\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\036\000\255\255\255\255\255\255\255\255\255\255\255\255\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\255\255\255\255\255\255\255\255\255\255\255\255\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\005\000\033\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\034\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255";
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
  | 2 -> Grammar.GE
  | 3 -> Grammar.GT
  | 4 -> Grammar.LE
  | 5 -> Grammar.LT
  | 6 -> Grammar.NEQ
  | 7 -> Grammar.EQ
  | 8 -> Grammar.DIVIDE
  | 9 -> Grammar.TIMES
  | 10 -> Grammar.MINUS
  | 11 -> Grammar.PLUS
  | 12 -> Grammar.DOT
  | 13 -> Grammar.LBRACE
  | 14 -> Grammar.RBRACE
  | 15 -> Grammar.LBRACK
  | 16 -> Grammar.RBRACK
  | 17 -> Grammar.LPAREN
  | 18 -> Grammar.RPAREN
  | 19 -> Grammar.ASSIGN
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
  | 30 -> raise End_of_file
  | __ocaml_lex_state ->
      lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf = __ocaml_lex_comment_rec lexbuf 34

and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
  | 0 -> token lexbuf
  | 1 -> comment lexbuf
  | __ocaml_lex_state ->
      lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

let parse lexbuf =
  let rec parse' lexbuf acc =
    let curr_token = token lexbuf in
    try parse' lexbuf (curr_token :: acc)
    with End_of_file -> curr_token :: acc
  in
  let res = List.rev (parse' lexbuf []) in
  if !ErrorMsg.anyErrors then raise ErrorMsg.Error else res

(* Unit tests *)
let%test_unit "parse_single_int" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "42" in
  [%test_eq: Grammar.token list] (parse lexbuf) [ Grammar.INT 42 ]

let%test_unit "parse_random_keywords" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "var type break" in
  [%test_eq: Grammar.token list] (parse lexbuf)
    [ Grammar.VAR; Grammar.TYPE; Grammar.BREAK ]

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
    ]

let%test_unit "parse_string" =
  ErrorMsg.reset ();
  let lexbuf = Lexing.from_string "\"A string\"" in
  [%test_eq: Grammar.token list] (parse lexbuf) [ Grammar.STRING "A string" ]

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
    (let test_dir = "../../../../tests/" in
     Caml.Sys.readdir test_dir |> Array.to_list
     |> List.filter ~f:(fun x -> String.(Caml.Filename.extension x = ".tig"))
     |> List.map ~f:(fun fname ->
            parse
              (Lexing.from_channel (Stdio.In_channel.create (test_dir ^ fname)))));
  [%expect]

(* let%test_unit "test" =
       ErrorMsg.reset ();
       let lexbuf = Lexing.from_string {|/* A program to solve the 8-queens problem */
   let
     type any = {any : int}
     var buffer := getchar()

     function readint(any: any) : int =
     let var i := 0
         function isdigit(s : string) : int =
           ord(buffer)>=ord("0") & ord(buffer)<=ord("9")
         function skipto() =
           while buffer=" " | buffer="\n"
             do buffer := getchar()
       in skipto();
         any.any := isdigit(buffer);
         while isdigit(buffer)
           do (i := i*10+ord(buffer)-ord("0"); buffer := getchar());
         i
     end

     type list = {first: int, rest: list}

     function readlist() : list =
         let var any := any{any=0}
             var i := readint(any)
         in if any.any
             then list{first=i,rest=readlist()}
             else nil
         end

     function merge(a: list, b: list) : list =
       if a=nil then b
       else if b=nil then a
       else if a.first < b.first
           then list{first=a.first,rest=merge(a.rest,b)}
           else list{first=b.first,rest=merge(a,b.rest)}

     function printint(i: int) =
       let function f(i:int) = if i>0
           then (f(i/10); print(chr(i-i/10*10+ord("0"))))
       in if i<0 then (print("-"); f(-i))
           else if i>0 then f(i)
           else print("0")
       end

     function printlist(l: list) =
       if l=nil then print("\n")
       else (printint(l.first); print(" "); printlist(l.rest))

       var list1 := readlist()
       var list2 := (buffer:=getchar(); readlist())


       /* BODY OF MAIN PROGRAM */
     in printlist(merge(list1,list2))
   end

   |} in
       [%test_eq: Grammar.token list] (parse lexbuf)
           [ Grammar.STRING "A string"; ] *)
