open Absyn
module A = Absyn

type token =
  | ID of string
  | STRING of string
  | INT of int
  | COMMA
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | AND
  | OR
  | ASSIGN
  | ARRAY
  | IF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | TO
  | DO
  | LET
  | IN
  | END
  | OF
  | BREAK
  | NIL
  | FUNCTION
  | VAR
  | TYPE
  | NEW
  | EOF

val input : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> A.exp
val sexp_of_token : token -> Base.Sexp.t
val compare_token : token -> token -> int
