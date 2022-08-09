open Symbol

module Types = struct
  (* To make each record/array unique *)
  type unique = unit ref

  type ty =
    | RECORD of (Symbol.symbol * ty) list * unique
    | NIL
    | INT
    | STRING
    | ARRAY of ty * unique
    | NAME of Symbol.symbol * ty option ref
    | UNIT

  (* TODO: Improve this, RECORD and NAME should more
     clearly describe its contents *)
  let to_string = function
    | RECORD _ -> "RECORD"
    | NIL -> "NIL"
    | INT -> "INT"
    | STRING -> "STRING"
    | ARRAY _ -> "ARRAY"
    | NAME _ -> "NAME"
    | UNIT -> "()"
end
