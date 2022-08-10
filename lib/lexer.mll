{
    open Errormsg
    open Base

    let create_hashtable size init =
        let tbl = Hashtbl.create ~size:size (module String) in
        List.iter ~f:(fun (key, data) -> ignore(Hashtbl.add tbl ~key:key ~data:data)) init;
        tbl

    let keyword_table =
        create_hashtable 8 [
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
        lexbuf.Lexing.lex_curr_p <- { pos with
            Lexing.pos_lnum = pos.Lexing.pos_lnum + 1; (* Increment line number *)
            Lexing.pos_bol = pos.Lexing.pos_cnum; (* Increment beginning of line *)
        };
        ErrorMsg.line_num := !ErrorMsg.line_num + 1;
        ErrorMsg.line_pos := (pos.Lexing.pos_cnum)::!ErrorMsg.line_pos;
    ;;
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    | digit+ as inum { Grammar.INT (Int.of_string inum) }
    | id as word
        {
            match Hashtbl.find keyword_table word with
            | Some tok -> tok
            | None -> Grammar.ID word
        }
    | ":=" { Grammar.ASSIGN }
    | ">=" { Grammar.GE }
    | ">" { Grammar.GT }
    | "<=" { Grammar.LE }
    | "<" { Grammar.LT }
    | "<>" { Grammar.NEQ }
    | "=" { Grammar.EQ }
    | "/" { Grammar.DIVIDE }
    | "*" { Grammar.TIMES }
    | "-" { Grammar.MINUS }
    | "+" { Grammar.PLUS }
    | "." { Grammar.DOT }
    | "{" { Grammar.LBRACE }
    | "}" { Grammar.RBRACE }
    | "[" { Grammar.LBRACK }
    | "]" { Grammar.RBRACK }
    | "(" { Grammar.LPAREN }
    | ")" { Grammar.RPAREN }
    | ":" { Grammar.COLON }
    | ";" { Grammar.SEMICOLON }
    | "," { Grammar.COMMA }
    | "&" { Grammar.AND }
    | "|" { Grammar.OR }
    | '"' [^ '"']* '"' as s { Grammar.STRING (String.sub s ~pos:1 ~len:(String.length s - 2)) }
    | "/*"
        (* Activate "comment" rule *)
        { comment lexbuf }
    | [' ' '\t' '\r'] { token lexbuf } (* eat up whitespace *)
    | '\n' { incr_linenum lexbuf; token lexbuf } (* eat up newline and increment line pos in the lexbuf *)
    (* Print an error message and skip the current char *)
    | _ { (ErrorMsg.error lexbuf "Invalid token"); token lexbuf }
    | eof { raise End_of_file }

and comment = parse
    | "*/"
        (* Reached the end of comment, return to the "token" rule *)
        { token lexbuf }
    | '\n' { incr_linenum lexbuf; comment lexbuf } (* eat up newline and increment line pos in the lexbuf *)
    | _ { comment lexbuf } (* Consume comments *)

{
    let parse lexbuf =
        let rec parse' lexbuf acc =
            let curr_token = token lexbuf in
            try parse' lexbuf (curr_token :: acc) with End_of_file -> (curr_token :: acc)
        in
        let res = List.rev (parse' lexbuf [])
        in
        if !ErrorMsg.anyErrors then
            raise ErrorMsg.Error
        else res

    (* Unit tests *)
    let%test_unit "parse_single_int" =
    ErrorMsg.reset ();
    let lexbuf = Lexing.from_string "42" in
    [%test_eq: Grammar.token list] (parse lexbuf)
        [ Grammar.INT 42; ]

    let%test_unit "parse_random_keywords" =
    ErrorMsg.reset ();
    let lexbuf = Lexing.from_string "var type break" in
    [%test_eq: Grammar.token list] (parse lexbuf)
        [
        Grammar.VAR;
        Grammar.TYPE;
        Grammar.BREAK;
        ]

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
    [%test_eq: Grammar.token list] (parse lexbuf)
        [ Grammar.STRING "A string"; ]

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
    ignore (let test_dir = "../../../tests/" in
        Caml.Sys.readdir test_dir |> Array.to_list
        |> List.filter ~f:(fun x -> String.(Caml.Filename.extension x = ".tig"))
        |> List.map ~f:(fun fname -> parse (Lexing.from_channel (Stdio.In_channel.create (test_dir ^ fname)))));
    [%expect]
}
