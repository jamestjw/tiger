open Base

module type Tiger_TOKENS = sig
  type token

  val _TYPE : int * int -> token
  val _VAR : int * int -> token
  val _FUNCTION : int * int -> token
  val _BREAK : int * int -> token
  val _OF : int * int -> token
  val _END : int * int -> token
  val _IN : int * int -> token
  val _NIL : int * int -> token
  val _LET : int * int -> token
  val _DO : int * int -> token
  val _TO : int * int -> token
  val _FOR : int * int -> token
  val _WHILE : int * int -> token
  val _ELSE : int * int -> token
  val _THEN : int * int -> token
  val _IF : int * int -> token
  val _ARRAY : int * int -> token
  val _ASSIGN : int * int -> token
  val _OR : int * int -> token
  val _AND : int * int -> token
  val _GE : int * int -> token
  val _GT : int * int -> token
  val _LE : int * int -> token
  val _LT : int * int -> token
  val _NEQ : int * int -> token
  val _EQ : int * int -> token
  val _DIVIDE : int * int -> token
  val _TIMES : int * int -> token
  val _MINUS : int * int -> token
  val _PLUS : int * int -> token
  val _DOT : int * int -> token
  val _RBRACE : int * int -> token
  val _LBRACE : int * int -> token
  val _RBRACK : int * int -> token
  val _LBRACK : int * int -> token
  val _RPAREN : int * int -> token
  val _LPAREN : int * int -> token
  val _SEMICOLON : int * int -> token
  val _COLON : int * int -> token
  val _COMMA : int * int -> token
  val _STRING : string * int * int -> token
  val _INT : int * int * int -> token
  val _ID : string * int * int -> token
  val _EOF : int * int -> token
  val is_eof : token -> bool
  val sexp_of_token : token -> Sexp.t
  val compare_token : token -> token -> int
end

module Tokens : Tiger_TOKENS = struct
  (* A "scaffold" structure for debugging lexers. *)
  type token = string

  let _TYPE (i, _) = "TYPE   " (* ^ Int.to_string i *)
  let _VAR (i, _) = "VAR   " (* ^ Int.to_string i *)
  let _FUNCTION (i, _) = "FUNCTION   " (* ^ Int.to_string i *)
  let _BREAK (i, _) = "BREAK   " (* ^ Int.to_string i *)
  let _OF (i, _) = "OF   " (* ^ Int.to_string i *)
  let _END (i, _) = "END   " (* ^ Int.to_string i *)
  let _IN (i, _) = "IN   " (* ^ Int.to_string i *)
  let _NIL (i, _) = "NIL   " (* ^ Int.to_string i *)
  let _LET (i, _) = "LET   " (* ^ Int.to_string i *)
  let _DO (i, _) = "DO   " (* ^ Int.to_string i *)
  let _TO (i, _) = "TO   " (* ^ Int.to_string i *)
  let _FOR (i, _) = "FOR   " (* ^ Int.to_string i *)
  let _WHILE (i, _) = "WHILE   " (* ^ Int.to_string i *)
  let _ELSE (i, _) = "ELSE   " (* ^ Int.to_string i *)
  let _THEN (i, _) = "THEN   " (* ^ Int.to_string i *)
  let _IF (i, _) = "IF   " (* ^ Int.to_string i *)
  let _ARRAY (i, _) = "ARRAY   " (* ^ Int.to_string i *)
  let _ASSIGN (i, _) = "ASSIGN   " (* ^ Int.to_string i *)
  let _OR (i, _) = "OR   " (* ^ Int.to_string i *)
  let _AND (i, _) = "AND   " (* ^ Int.to_string i *)
  let _GE (i, _) = "GE   " (* ^ Int.to_string i *)
  let _GT (i, _) = "GT   " (* ^ Int.to_string i *)
  let _LE (i, _) = "LE   " (* ^ Int.to_string i *)
  let _LT (i, _) = "LT   " (* ^ Int.to_string i *)
  let _NEQ (i, _) = "NEQ   " (* ^ Int.to_string i *)
  let _EQ (i, _) = "EQ   " (* ^ Int.to_string i *)
  let _DIVIDE (i, _) = "DIVIDE   " (* ^ Int.to_string i *)
  let _TIMES (i, _) = "TIMES   " (* ^ Int.to_string i *)
  let _MINUS (i, _) = "MINUS   " (* ^ Int.to_string i *)
  let _PLUS (i, _) = "PLUS   " (* ^ Int.to_string i *)
  let _DOT (i, _) = "DOT   " (* ^ Int.to_string i *)
  let _RBRACE (i, _) = "RBRACE   " (* ^ Int.to_string i *)
  let _LBRACE (i, _) = "LBRACE   " (* ^ Int.to_string i *)
  let _RBRACK (i, _) = "RBRACK   " (* ^ Int.to_string i *)
  let _LBRACK (i, _) = "LBRACK   " (* ^ Int.to_string i *)
  let _RPAREN (i, _) = "RPAREN   " (* ^ Int.to_string i *)
  let _LPAREN (i, _) = "LPAREN   " (* ^ Int.to_string i *)
  let _SEMICOLON (i, _) = "SEMICOLON   " (* ^ Int.to_string i *)
  let _COLON (i, _) = "COLON   " (* ^ Int.to_string i *)
  let _COMMA (i, _) = "COMMA   " (* ^ Int.to_string i *)
  let _STRING (s, i, _) = "STRING(" ^ s ^ ")     " (* ^ Int.to_string i *)
  let _INT (c, i, _) = "INT(" ^ Int.to_string c ^ ")   " (* ^ Int.to_string i *)
  let _ID (s, i, _) = "ID(" ^ s ^ ")     " (* ^ Int.to_string i *)
  let _EOF (i, _) = "EOF   " (* ^ Int.to_string i *)
  let is_eof tok = String.(String.sub tok ~pos:0 ~len:3 = "EOF")
  let sexp_of_token t = Sexp.List [ Sexp.Atom t ]
  let compare_token tok1 tok2 = String.compare tok1 tok2
end
