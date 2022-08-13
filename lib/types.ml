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

  let equals = function
    | RECORD (_, u1), RECORD (_, u2) -> u1 = u2
    | ARRAY (_, u1), ARRAY (_, u2) -> u1 = u2
    | NIL, NIL -> true
    | INT, INT -> true
    | STRING, STRING -> true
    | UNIT, UNIT -> true
    | NAME (n1, _), NAME (n2, _) -> n1 = n2
    (* NIL is compatible with all RECORDS *)
    | NIL, RECORD _ -> true
    | RECORD _, NIL -> true
    | _ -> false
end
