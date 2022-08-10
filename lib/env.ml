open Symbol
open Types

module Env = struct
  type ty = Types.ty

  type enventry =
    | VarEntry of { ty : ty }
    | FunEntry of { formals : ty list; result : ty }

  let base_tenv =
    let types = [ ("int", Types.INT); ("string", Types.STRING) ] in
    List.fold_left
      (fun env (name, ty) -> Symbol.enter (env, Symbol.to_symbol name, ty))
      Symbol.empty types

  let base_venv =
    (* TODO: Check if we need the param names in this list *)
    let functions =
      [
        ("print", [ ("s", Types.STRING) ], Types.NIL);
        ("flush", [], Types.NIL);
        ("getchar", [], Types.STRING);
        ("ord", [ ("s", Types.STRING) ], Types.INT);
        ("chr", [ ("i", Types.INT) ], Types.STRING);
        ("size", [ ("s", Types.STRING) ], Types.INT);
        ( "substring",
          [ ("s", Types.STRING); ("first", Types.INT); ("n", Types.INT) ],
          Types.STRING );
        ("concat", [ ("s1", Types.STRING); ("s2", Types.STRING) ], Types.STRING);
        ("not", [ ("i", Types.INT) ], Types.INT);
        ("exit", [ ("i", Types.INT) ], Types.NIL);
      ]
    in
    List.fold_left
      (fun env (name, formals, ret_type) ->
        Symbol.enter
          ( env,
            Symbol.to_symbol name,
            FunEntry
              {
                formals = List.map (fun (_, t) -> t) formals;
                result = ret_type;
              } ))
      Symbol.empty functions
end
