%{
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

/* Used to resolve shift-reduce conflict (dangling else) */
%nonassoc THEN
%nonassoc ELSE

%left LOW_PREC /* Used to resolve shift-reduce conflicts (TODO: Find a better name) */
%left AND OR
%left EQ NEQ
%left LT GT LE GE
%left PLUS MINUS
%left TIMES DIVIDE
%left NEG  /* negation -- unary minus */
%left LBRACK LPAREN /* Array access and fncall */
%right ASSIGN

%start input
%type <unit> input

%% /* Grammar rules and actions */
input: /* empty */ {}
    | input statement {}

statement: let_stmt {}
    /*
     * SR conflict here when a SEMICOLON is encountered
     * but it is fine as we a shift is favored, i.e. the
     * SEMICOLON is always consumed to form a longer stmt.
     */
    | nonempty_expseq {}

decs: dec {}
    | decs dec {}

dec: tydec {}
    | vardec {}
    | fundec {}

tydec: TYPE ID EQ ty {}

ty: ID {}
    | LBRACE tyfields RBRACE {}
    | ARRAY OF ID {}

tyfields: /* empty */ {}
    | nonempty_tyfields {}

/* tyfields with length > 0 */
nonempty_tyfields : tyfield {}
    | nonempty_tyfields COMMA tyfield {}

tyfield: ID COLON ID {}

record_initialisation_fields: /* empty */ {}
    | ne_record_initialisation_fields {}

ne_record_initialisation_fields: record_initialisation_field {}
    | ne_record_initialisation_fields COMMA record_initialisation_field {}

record_initialisation_field: ID EQ exp {}

vardec: VAR ID ASSIGN exp {}
    | VAR ID COLON ID ASSIGN exp {}

fundec: FUNCTION ID LPAREN tyfields RPAREN EQ statement {} /* function id (tyfields) = exp */
    | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ statement {} /* function id (tyfields) : type-id = exp */

let_stmt: LET decs IN expseq END {}

expseq: /* empty */ {}
    | nonempty_expseq {}

/* expseq with length > 0 */
nonempty_expseq: exp %prec LOW_PREC {}
    | nonempty_expseq SEMICOLON exp %prec LOW_PREC {}

exp: STRING {}
    | ID %prec LOW_PREC {}
    | INT {}
    | NIL {}
    | exp PLUS exp {}
    | exp MINUS exp {}
    | exp TIMES exp {}
    | exp DIVIDE exp {}
    | exp EQ  exp {}
    | exp NEQ exp {}
    | exp LT  exp {}
    | exp GT  exp {}
    | exp LE  exp {}
    | exp GE  exp {}
    | exp AND exp {}
    | exp OR  exp {}
    | MINUS exp %prec NEG {}
    | BREAK {}
    | assignment {}
    | funcall {}
    | ID DOT ID {}
    | ID LBRACK exp RBRACK {} /* Indexing into array */
    | ID LBRACK exp RBRACK OF exp %prec LOW_PREC {} /* Initialising new array */
    | ID LBRACE record_initialisation_fields RBRACE {} /* Initialising new record */
    | if_stmt {}
    | for_stmt {}
    | while_stmt {}
    | LPAREN expseq RPAREN {}

assignment: exp ASSIGN exp {}

while_stmt: WHILE exp DO statement {}

/* Conflict resolution of shifting gives the desired behaviour */
if_stmt: IF exp THEN statement {}
    | IF exp THEN statement ELSE statement {}

for_stmt: FOR ID ASSIGN exp TO exp DO statement {}

funcall: ID LPAREN params RPAREN {}

params: /* empty */ {}
    | non_empty_params {}

non_empty_params: exp {}
    | non_empty_params COMMA exp {}
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

let compare_token t1 t2 =
  if Base.Poly.(t1 = t2) then 0
  else 1 (* always return 1 if not equal since ordering is not defined *)
