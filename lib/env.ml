open Symbol
open Types
open Translate
open Temp

module Env = struct
  type ty = Types.ty

  type enventry =
    | VarEntry of { ty : ty; access : Translate.access }
    | FunEntry of {
        formals : ty list;
        result : ty;
        level : Translate.level;
        label : Temp.label;
      }

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
        ("printint", [ ("s", Types.INT) ], Types.NIL);
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
        let label = Temp.named_label name in
        Symbol.enter
          ( env,
            Symbol.to_symbol name,
            FunEntry
              {
                formals = List.map (fun (_, t) -> t) formals;
                result = ret_type;
                level =
                  Translate.new_level ~parent:Translate.outermost
                    ~formals:(List.map (fun _ -> false) formals)
                    ~name:label ~static:false;
                label;
              } ))
      Symbol.empty functions
end
