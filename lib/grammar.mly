%{
    open Absyn
    open Symbol
    open Errormsg
    module A  = Absyn

    let get_pos_cnum () = (Parsing.symbol_start_pos ()).pos_cnum
    let get_pos_cnum_of_n n = (Parsing.rhs_start_pos n).pos_cnum

    exception InternalError
%}

%token <string> ID STRING
%token <int> INT
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
%token LBRACE RBRACE DOT
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
%token AND OR ASSIGN
%token ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
%token BREAK NIL
%token FUNCTION VAR TYPE
%token EOF

/* Used to resolve shift-reduce conflict (dangling else) */
%nonassoc THEN
%nonassoc ELSE

%left LOW_PREC /* Used to resolve shift-reduce conflicts (TODO: Find a better name) */
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LE GE
%left PLUS MINUS
%left TIMES DIVIDE
%left NEG  /* negation -- unary minus */
%left LBRACK LPAREN DOT /* Array access and fncall */

%start input
%type <A.exp> input

%% /* Grammar rules and actions */
input: exp EOF { $1 }

decs: /* empty */ { [] }
    | decs dec { $1 @ [$2] }

dec: tydec { $1 }
    | vardec { $1 }
    | fundec { $1 }

tydec: TYPE ID EQ ty { A.TypeDec { name = (Symbol.to_symbol $2); ty = $4; pos = (get_pos_cnum ()) } }

ty: ID { A.NameTy ((Symbol.to_symbol $1), (get_pos_cnum ())) }
    | LBRACE tyfields RBRACE { A.RecordTy $2 }
    | ARRAY OF ID { A.ArrayTy ((Symbol.to_symbol $3), (get_pos_cnum ())) }

tyfields: /* empty */ { [] }
    | nonempty_tyfields { $1 }

/* tyfields with length > 0 */
nonempty_tyfields : tyfield { [$1] }
    | nonempty_tyfields COMMA tyfield { $1 @ [$3] }

tyfield: ID COLON ID {
    { 
        name = (Symbol.to_symbol $1);
        escape = ref true;
        typ = (Symbol.to_symbol $3);
        pos = (get_pos_cnum ())
    }
}

record_initialisation_fields: /* empty */ { [] }
    | ne_record_initialisation_fields { $1 }

ne_record_initialisation_fields: record_initialisation_field { [$1] }
    | ne_record_initialisation_fields COMMA record_initialisation_field { $1 @ [$3] }

record_initialisation_field: ID EQ exp { ((Symbol.to_symbol $1), $3, (get_pos_cnum ())) }

vardec: VAR ID ASSIGN exp {
        VarDec {
            name = (Symbol.to_symbol $2);
            escape = ref true;
            typ = None;
            init = $4;
            pos = (get_pos_cnum ());
        }
    }
    | VAR ID COLON ID ASSIGN exp {
        VarDec {
            name = (Symbol.to_symbol $2);
            escape = ref true;
            typ = Some ((Symbol.to_symbol $4), (get_pos_cnum_of_n 4));
            init = $6;
            pos = (get_pos_cnum ());
        }
     }

/* function id (tyfields) = exp */
fundec: FUNCTION ID LPAREN tyfields RPAREN EQ exp {
        FunctionDec {
            name = (Symbol.to_symbol $2);
            params = $4;
            result = None;
            body = $7;
            pos = (get_pos_cnum ());
        }
    }
    /* function id (tyfields) : type-id = exp */
    | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {
        FunctionDec {
            name = (Symbol.to_symbol $2);
            params = $4;
            result = Some ((Symbol.to_symbol $7), (get_pos_cnum_of_n 7));
            body = $9;
            pos = (get_pos_cnum ());
        }
    }

let_stmt: LET decs IN expseq END { 
    A.LetExp {
        decs = $2;
        body = $4;
        pos = (get_pos_cnum ())
    }
 }

expseq: /* empty */ { A.SeqExp [] }
    | nonempty_expseq { $1 }

/* expseq with length > 0 */
nonempty_expseq: exp %prec LOW_PREC { A.SeqExp [($1, (get_pos_cnum ()))] }
    | nonempty_expseq SEMICOLON exp %prec LOW_PREC {
        match $1 with
        | A.SeqExp l -> A.SeqExp (l @ [($3, (get_pos_cnum ()))])
        | _ -> raise InternalError (* This can never happen *)
     }
    /* TODO: Verify correctness of AST when an error is encountered */
    | error SEMICOLON exp %prec LOW_PREC {
        ErrorMsg.error_pos (get_pos_cnum ()) "Syntax error";
        A.SeqExp [($3, (get_pos_cnum ()))]
     }

exp: STRING { A.StringExp ($1, (get_pos_cnum ())) }
    | INT { A.IntExp $1 }
    | NIL { A.NilExp (get_pos_cnum ()) }
    | exp PLUS exp { A.OpExp { left = $1; right = $3; oper = A.PlusOp; pos = (get_pos_cnum ()) } }
    | exp MINUS exp { A.OpExp { left = $1; right = $3; oper = A.MinusOp; pos = (get_pos_cnum ()) } }
    | exp TIMES exp { A.OpExp { left = $1; right = $3; oper = A.TimesOp; pos = (get_pos_cnum ()) } }
    | exp DIVIDE exp { A.OpExp { left = $1; right = $3; oper = A.DivideOp; pos = (get_pos_cnum ()) } }
    | exp EQ  exp { A.OpExp { left = $1; right = $3; oper = A.EqOp; pos = (get_pos_cnum ()) } }
    | exp NEQ exp { A.OpExp { left = $1; right = $3; oper = A.NeqOp; pos = (get_pos_cnum ()) } }
    | exp LT  exp { A.OpExp { left = $1; right = $3; oper = A.LtOp; pos = (get_pos_cnum ()) } }
    | exp GT  exp { A.OpExp { left = $1; right = $3; oper = A.GtOp; pos = (get_pos_cnum ()) } }
    | exp LE  exp { A.OpExp { left = $1; right = $3; oper = A.LeOp; pos = (get_pos_cnum ()) } }
    | exp GE  exp { A.OpExp { left = $1; right = $3; oper = A.GeOp; pos = (get_pos_cnum ()) } }
    /* Express e1 & e2 as if e1 then e2 else 0 */
    | exp AND exp { A.IfExp { test = $1; then' = $3; else' = Some (A.IntExp 0); pos = (get_pos_cnum ()) } }
    /* Express e1 | e2 as if e1 then 1 else e2 */
    | exp OR  exp { A.IfExp { test = $1; then' = A.IntExp 1; else' = Some $3; pos = (get_pos_cnum ()) } }
    /* Unary negation, express -i as (0 - i) */
    | MINUS exp %prec NEG { A.OpExp { left = A.IntExp 0; right = $2; oper = A.MinusOp; pos = (get_pos_cnum ()) } }
    | BREAK { A.BreakExp (get_pos_cnum ()) }
    | assignment { $1 }
    | funcall { $1 }
    | var { A.VarExp $1 }
    /* Initialising new array */
    | ID LBRACK exp RBRACK OF exp %prec LOW_PREC {
        A.ArrayExp {
            typ = (Symbol.to_symbol $1); 
            size = $3;
            init = $6;
            pos = (get_pos_cnum ())
        }
     }
    /* Initialising new record */
    | ID LBRACE record_initialisation_fields RBRACE {
        A.RecordExp {
            fields = $3;
            typ = (Symbol.to_symbol $1); 
            pos = (get_pos_cnum ())
        }
     }
    | if_stmt { $1 }
    | for_stmt { $1 }
    | while_stmt { $1 }
    | LPAREN expseq RPAREN { $2 }
    /* TODO: Figure out what to return for error production */
    | LPAREN error RPAREN {
        ErrorMsg.error_pos (get_pos_cnum_of_n 2) "Syntax error";
        A.NilExp (get_pos_cnum_of_n 2)
     }
    | let_stmt { $1 }

var: ID DOT ID {
        A.FieldVar (
        (A.SimpleVar ((Symbol.to_symbol $1), (get_pos_cnum ()))),
        (Symbol.to_symbol $3),
        (get_pos_cnum ()))
    }
    /* Indexing into array */
    | ID LBRACK exp RBRACK {
        A.SubscriptVar (
            (A.SimpleVar ((Symbol.to_symbol $1), (get_pos_cnum ()))),
            $3,
            (get_pos_cnum ()))
     }
    | ID %prec LOW_PREC { A.SimpleVar ((Symbol.to_symbol $1), (get_pos_cnum ())) }

assignment: var ASSIGN exp { 
    AssignExp { var = $1; exp = $3; pos = (get_pos_cnum ()) }
 }

while_stmt: WHILE exp DO exp %prec LOW_PREC {
    WhileExp { test = $2; body = $4; pos = (get_pos_cnum ()) }
}

/* Conflict resolution of shifting gives the desired behaviour */
if_stmt: IF exp THEN exp{
       A.IfExp { test = $2; then' = $4; else' = None; pos = (get_pos_cnum ()) }
     }
    | IF exp THEN exp ELSE exp { 
        A.IfExp { test = $2; then' = $4; else' = Some $6; pos = (get_pos_cnum ()) }
     }

for_stmt: FOR ID ASSIGN exp TO exp DO exp %prec LOW_PREC { 
    ForExp {
        var = (Symbol.to_symbol $2);
        escape = ref true;
        lo = $4;
        hi = $6;
        body = $8;
        pos = (get_pos_cnum ()) 
    }
 }

funcall: ID LPAREN params RPAREN {
    CallExp {
        func = (Symbol.to_symbol $1);
        args = $3;
        pos = (get_pos_cnum ())
    }
}

params: /* empty */ { [] }
    | non_empty_params { $1 }

non_empty_params: exp { [$1] }
    | non_empty_params COMMA exp { $1 @ [$3] }
%%

let sexp_of_token t =
  match t with
  | ID s -> Base.Sexp.List [ Base.Sexp.Atom "ID"; Base.Sexp.Atom s ]
  | STRING s -> Base.Sexp.List [ Base.Sexp.Atom "STRING"; Base.Sexp.Atom s ]
  | INT i ->
      Base.Sexp.List [ Base.Sexp.Atom "INT"; Base.Sexp.Atom (string_of_int i) ]
  | COMMA -> Base.Sexp.List [ Base.Sexp.Atom "COMMA" ]
  | COLON -> Base.Sexp.List [ Base.Sexp.Atom "COLON" ]
  | SEMICOLON -> Base.Sexp.List [ Base.Sexp.Atom "SEMICOLON" ]
  | LPAREN -> Base.Sexp.List [ Base.Sexp.Atom "LPAREN" ]
  | RPAREN -> Base.Sexp.List [ Base.Sexp.Atom "RPAREN" ]
  | LBRACK -> Base.Sexp.List [ Base.Sexp.Atom "LBRACK" ]
  | RBRACK -> Base.Sexp.List [ Base.Sexp.Atom "RBRACK" ]
  | LBRACE -> Base.Sexp.List [ Base.Sexp.Atom "LBRACE" ]
  | RBRACE -> Base.Sexp.List [ Base.Sexp.Atom "RBRACE" ]
  | DOT -> Base.Sexp.List [ Base.Sexp.Atom "DOT" ]
  | PLUS -> Base.Sexp.List [ Base.Sexp.Atom "PLUS" ]
  | MINUS -> Base.Sexp.List [ Base.Sexp.Atom "MINUS" ]
  | TIMES -> Base.Sexp.List [ Base.Sexp.Atom "TIMES" ]
  | DIVIDE -> Base.Sexp.List [ Base.Sexp.Atom "DIVIDE" ]
  | EQ -> Base.Sexp.List [ Base.Sexp.Atom "EQ" ]
  | NEQ -> Base.Sexp.List [ Base.Sexp.Atom "NEQ" ]
  | LT -> Base.Sexp.List [ Base.Sexp.Atom "LT" ]
  | LE -> Base.Sexp.List [ Base.Sexp.Atom "LE" ]
  | GT -> Base.Sexp.List [ Base.Sexp.Atom "GT" ]
  | GE -> Base.Sexp.List [ Base.Sexp.Atom "GE" ]
  | AND -> Base.Sexp.List [ Base.Sexp.Atom "AND" ]
  | OR -> Base.Sexp.List [ Base.Sexp.Atom "OR" ]
  | ASSIGN -> Base.Sexp.List [ Base.Sexp.Atom "ASSIGN" ]
  | ARRAY -> Base.Sexp.List [ Base.Sexp.Atom "ARRAY" ]
  | IF -> Base.Sexp.List [ Base.Sexp.Atom "IF" ]
  | THEN -> Base.Sexp.List [ Base.Sexp.Atom "THEN" ]
  | ELSE -> Base.Sexp.List [ Base.Sexp.Atom "ELSE" ]
  | WHILE -> Base.Sexp.List [ Base.Sexp.Atom "WHILE" ]
  | FOR -> Base.Sexp.List [ Base.Sexp.Atom "FOR" ]
  | TO -> Base.Sexp.List [ Base.Sexp.Atom "TO" ]
  | DO -> Base.Sexp.List [ Base.Sexp.Atom "DO" ]
  | LET -> Base.Sexp.List [ Base.Sexp.Atom "LET" ]
  | IN -> Base.Sexp.List [ Base.Sexp.Atom "IN" ]
  | END -> Base.Sexp.List [ Base.Sexp.Atom "END" ]
  | OF -> Base.Sexp.List [ Base.Sexp.Atom "OF" ]
  | BREAK -> Base.Sexp.List [ Base.Sexp.Atom "BREAK" ]
  | NIL -> Base.Sexp.List [ Base.Sexp.Atom "NIL" ]
  | FUNCTION -> Base.Sexp.List [ Base.Sexp.Atom "FUNCTION" ]
  | VAR -> Base.Sexp.List [ Base.Sexp.Atom "VAR" ]
  | TYPE -> Base.Sexp.List [ Base.Sexp.Atom "TYPE" ]
  | EOF -> Base.Sexp.List [ Base.Sexp.Atom "EOF" ]

let compare_token t1 t2 =
  if Base.Poly.(t1 = t2) then 0
  else 1 (* always return 1 if not equal since ordering is not defined *)
