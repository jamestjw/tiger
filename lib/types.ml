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

  let rec to_string = function
    | RECORD (fields, _) ->
        let field_to_string (s, t) =
          Printf.sprintf "%s: %s" (Symbol.name s) (to_string t)
        in
        Printf.sprintf "RECORD{%s}"
          (String.concat ", " (List.map field_to_string fields))
    | NIL -> "NIL"
    | INT -> "INT"
    | STRING -> "STRING"
    | ARRAY (t, _) -> Printf.sprintf "ARRAY<%s>" (to_string t)
    | NAME (s, _) -> Printf.sprintf "NAME<%s>" (Symbol.name s)
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

open Base

let%test_unit "simple_record_to_string" =
  let expected = "RECORD{x: INT, y: INT}" in
  let t =
    Types.RECORD
      ( [ (Symbol.to_symbol "x", Types.INT); (Symbol.to_symbol "y", Types.INT) ],
        ref () )
  in
  [%test_eq: string] (Types.to_string t) expected

let%test_unit "record_with_name_to_string" =
  let expected = "RECORD{x: INT, y: NAME<CUSTOM>}" in
  let t =
    Types.RECORD
      ( [
          (Symbol.to_symbol "x", Types.INT);
          ( Symbol.to_symbol "y",
            Types.NAME (Symbol.to_symbol "CUSTOM", ref None) );
        ],
        ref () )
  in
  [%test_eq: string] (Types.to_string t) expected

let%test_unit "array_with_name_to_string" =
  let expected = "ARRAY<NAME<CUSTOM>>" in
  let t =
    Types.ARRAY (Types.NAME (Symbol.to_symbol "CUSTOM", ref None), ref ())
  in
  [%test_eq: string] (Types.to_string t) expected
