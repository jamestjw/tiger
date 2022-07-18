{
    open Tokens
    open Errormsg
    open Base

    let create_hashtable size init =
        let tbl = Hashtbl.create ~size:size (module String) in
        List.iter ~f:(fun (key, data) -> ignore(Hashtbl.add tbl ~key:key ~data:data)) init;
        tbl

    let keyword_table =
        create_hashtable 8 [
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

    let incr_linenum lexbuf =
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with
            Lexing.pos_lnum = pos.Lexing.pos_lnum + 1; (* Increment line number *)
            Lexing.pos_bol = pos.Lexing.pos_cnum + 1; (* Increment beginning of line *)
        }
    ;;
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' '0'-'9']*

rule token = parse
    | digit+ as inum { Tokens._INT (Int.of_string inum, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | id as word
        {
            match Hashtbl.find keyword_table word with
            | Some token_fn -> token_fn (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
            | None -> Tokens._ID (word, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
        }
    | ">=" { Tokens._GE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | ">" { Tokens._GT (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "<=" { Tokens._LE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "<" { Tokens._LT (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "!=" { Tokens._NEQ (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "=" { Tokens._EQ (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "/" { Tokens._DIVIDE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "*" { Tokens._TIMES (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "-" { Tokens._MINUS (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "+" { Tokens._PLUS (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "." { Tokens._DOT (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "{" { Tokens._LBRACE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "}" { Tokens._RBRACE (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "[" { Tokens._LBRACK (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "]" { Tokens._RBRACK (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "(" { Tokens._LPAREN (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | ")" { Tokens._RPAREN (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | ":" { Tokens._COLON (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | ";" { Tokens._SEMICOLON (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "," { Tokens._COMMA (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | '"' [^ '"']* '"' as s { Tokens._STRING ((String.sub s ~pos:1 ~len:(String.length s - 2)), Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }
    | "/*"
        (* Activate "comment" rule *)
        { comment lexbuf }
    | [' ' '\t'] { token lexbuf } (* eat up whitespace *)
    |  '\n' { incr_linenum lexbuf; token lexbuf } (* eat up newline and increment line pos in the lexbuf *)
    (* Print an error message and skip the current char *)
    | _ { (ErrorMsg.error lexbuf "Invalid token"); token lexbuf }
    | eof { Tokens._EOF (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) }

and comment = parse
    | "*/"
        (* Reached the end of comment, return to the "token" rule *)
        { token lexbuf }
    | _ { comment lexbuf } (* Consume comments *)

{
    let parse lexbuf =
    let rec parse' lexbuf acc =
        let curr_token = token lexbuf in
        let res = curr_token :: acc in
        if Tokens.is_eof curr_token then res else parse' lexbuf res
    in
    let res = List.rev (parse' lexbuf [])
    in
    if !ErrorMsg.anyErrors then
        raise ErrorMsg.Error
    else res

    (* Unit tests *)
    let%test_unit "parse_single_int" =
    let lexbuf = Lexing.from_string "42" in
    [%test_eq: Tokens.token list] (parse lexbuf)
        [ Tokens._INT (42, 0, 1); Tokens._EOF (2, 2) ]

    let%test_unit "parse_random_keywords" =
    let lexbuf = Lexing.from_string "var type break" in
    [%test_eq: Tokens.token list] (parse lexbuf)
        [
        Tokens._VAR (0, 0);
        Tokens._TYPE (2, 2);
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

    exception Missing_exception

    let%expect_test "parse_invalid_symbol" =
    let lexbuf = Lexing.from_string "let type arrtype = ~array of" in
    try 
        ignore (parse lexbuf); 
        raise Missing_exception
    with ErrorMsg.Error -> 
        ();
        [%expect {| :1.21: Invalid token |}]

    let%expect_test "parse_invalid_symbol_line2" =
    let lexbuf = Lexing.from_string "let type arrtype = array of\ncat ` hello" in
    try 
        ignore (parse lexbuf); 
        raise Missing_exception
    with ErrorMsg.Error -> 
        ();
        [%expect {| :2.5: Invalid token |}]

    let%expect_test "parse_invalid_symbol_line3" =
    let lexbuf = Lexing.from_string "let type\ncat hello\nwrong $ token" in
    try 
        ignore (parse lexbuf); 
        raise Missing_exception
    with ErrorMsg.Error -> 
        ();
        [%expect {| :3.7: Invalid token |}]
}
