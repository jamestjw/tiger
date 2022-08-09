open Symbol

module Absyn = struct
  type symbol = Symbol.symbol

  type var =
    | SimpleVar of symbol * int
    | FieldVar of var * symbol * int
    | SubscriptVar of var * exp * int

  and exp =
    | VarExp of var
    | NilExp of int
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

  let var_pos = function
    | SimpleVar (_, pos) -> pos
    | FieldVar (_, _, pos) -> pos
    | SubscriptVar (_, _, pos) -> pos

  let exp_pos = function
    | VarExp var -> var_pos var
    | NilExp pos -> pos
    | IntExp pos -> pos
    | StringExp (_, pos) -> pos
    | CallExp { pos; _ } -> pos
    | OpExp { pos; _ } -> pos
    | RecordExp { pos; _ } -> pos
    | SeqExp [] -> 0
    | SeqExp ((_, pos) :: _) -> pos
    | AssignExp { pos; _ } -> pos
    | IfExp { pos; _ } -> pos
    | WhileExp { pos; _ } -> pos
    | ForExp { pos; _ } -> pos
    | BreakExp pos -> pos
    | LetExp { pos; _ } -> pos
    | ArrayExp { pos; _ } -> pos
end
