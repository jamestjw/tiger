open Symbol

module Absyn = struct
  type symbol = Symbol.symbol

  type var =
    | SimpleVar of symbol * int
    | FieldVar of var * symbol * int
    | SubscriptVar of var * exp * int

  and exp =
    | VarExp of var
    | NilExp
    | IntExp of int
    | StringExp of string * int
    | CallExp of { func : symbol; args : exp list; pos : int }
    | OpExp of { left : exp; oper : oper; right : exp; pos : int }
    | RecordExp of {
        fields : (symbol * exp * int) list;
        typ : symbol;
        pos : int;
      }
    | SeqExp of (exp * int) list
    | AssignExp of { var : var; exp : exp; pos : int }
    | IfExp of { test : exp; then' : exp; else' : exp option; pos : int }
    | WhileExp of { test : exp; body : exp; pos : int }
    | ForExp of {
        var : symbol;
        escape : bool ref;
        lo : exp;
        hi : exp;
        body : exp;
        pos : int;
      }
    | BreakExp of int
    | LetExp of { decs : dec list; body : exp; pos : int }
    | ArrayExp of { typ : symbol; size : exp; init : exp; pos : int }

  and var_dec_field = { name : symbol; ty : ty; pos : int }

  and dec =
    | FunctionDec of fundec
    | VarDec of {
        name : symbol;
        escape : bool ref;
        typ : (symbol * int) option;
        init : exp;
        pos : int;
      }
    | TypeDec of var_dec_field

  and ty =
    | NameTy of symbol * int
    | RecordTy of field list
    | ArrayTy of symbol * int

  and oper =
    | PlusOp
    | MinusOp
    | TimesOp
    | DivideOp
    | EqOp
    | NeqOp
    | LtOp
    | LeOp
    | GtOp
    | GeOp

  and field = { name : symbol; escape : bool ref; typ : symbol; pos : int }

  and fundec = {
    name : symbol;
    params : field list;
    result : (symbol * int) option;
    body : exp;
    pos : int;
  }
end
