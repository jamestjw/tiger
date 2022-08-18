open Absyn
open Symbol
module A = Absyn

module FindEscape : sig
  val find_escape : Absyn.exp -> unit
end = struct
  type depth = int
  type esc_env = (depth * bool ref) Symbol.tbl

  let rec traverseVar (env, d, s) =
    let lookup_and_set_true name =
      match Symbol.look (env, name) with
      (* If the variable is accessed in a depth greater than which
         it was defined in, set escape to true *)
      | Some (d', r) -> if d > d' then r := true
      (* If an undefined variable was referenced, we leave the
         error reporting to the semantic analysis phase *)
      | None ->
          Errormsg.ErrorMsg.error_pos (A.var_pos s)
            ("Undefined variable !!:" ^ Symbol.name name)
    in
    match s with
    | A.SimpleVar (s, _) -> lookup_and_set_true s
    | A.FieldVar (v, _, _) -> traverseVar (env, d, v)
    | A.SubscriptVar (v, e, _) ->
        traverseVar (env, d, v);
        traverseExp (env, d, e)

  and traverseExp ((env : esc_env), (d : depth), (s : A.exp)) : unit =
    (* Syntactic sugar to prevent passing in env and depth all the time *)
    let trexp s = traverseExp (env, d, s) in
    match s with
    | VarExp var -> traverseVar (env, d, var)
    | CallExp { args; _ } -> List.iter trexp args
    | OpExp { left; right; _ } ->
        trexp left;
        trexp right
    | RecordExp { fields; _ } -> List.iter (fun (_, e, _) -> trexp e) fields
    | SeqExp exps -> List.iter (fun (e, _) -> trexp e) exps
    | AssignExp { var; exp; _ } ->
        traverseVar (env, d, var);
        trexp exp
    | IfExp { test; then'; else'; _ } -> (
        trexp test;
        trexp then';
        match else' with Some e -> trexp e | None -> ())
    | WhileExp { test; body; _ } ->
        trexp test;
        trexp body
    | ForExp { var; escape = r; lo; hi; body; _ } ->
        (* Put the loop variable in the environment *)
        r := false;
        let env' = Symbol.enter (env, var, (d, r)) in
        trexp lo;
        trexp hi;
        (* Traverse the body with the new environment, the body could
           contain functions that might cause the variable to escape *)
        traverseExp (env', d, body)
    | LetExp { decs; body; _ } ->
        let env' = traverseDecs (env, d, decs) in
        traverseExp (env', d, body)
    | ArrayExp { size; init; _ } ->
        trexp size;
        trexp init
    | _ -> ()

  and traverseDecs ((env : esc_env), (d : depth), (s : A.dec list)) =
    let set_false_and_insert e name d r =
      r := false;
      Symbol.enter (e, name, (d, r))
    in
    let traverseDec env dec =
      match dec with
      | A.VarDec { name; escape = r; init; _ } ->
          traverseExp (env, d, init);
          set_false_and_insert env name d r
      | A.FunctionDec { params; body; _ } ->
          (* Body and params are evaluated in the new depth *)
          let d' = d + 1 in
          let env' =
            List.fold_left
              (fun e (f : A.field) -> set_false_and_insert e f.name d' f.escape)
              env params
          in
          traverseExp (env', d', body);
          env'
      | TypeDec _ -> env
      (* Do nothing since type declarations have nothing to do with escaped variables *)
    in
    List.fold_left traverseDec env s

  let find_escape exp = traverseExp (Symbol.empty, 0, exp)
end

open Base

let%test_unit "find_escaped_vars_simple_let" =
  [%test_eq: A.exp]
    (let exp =
       Parser.parse_string
         {|
      let
        var escaped := 8
        var unescaped := 5
        function getN(): int = escaped
        in
        getN()
      end 
    |}
     in
     FindEscape.find_escape exp;
     exp)
    (A.LetExp
       {
         decs =
           [
             A.VarDec
               {
                 name = Symbol.to_symbol "escaped";
                 escape = ref true;
                 typ = None;
                 init = A.IntExp 8;
                 pos = 19;
               };
             A.VarDec
               {
                 name = Symbol.to_symbol "unescaped";
                 escape = ref false;
                 typ = None;
                 init = A.IntExp 5;
                 pos = 44;
               };
             A.FunctionDec
               {
                 name = Symbol.to_symbol "getN";
                 params = [];
                 result = Some (Symbol.to_symbol "int", 88);
                 body = A.VarExp (A.SimpleVar (Symbol.to_symbol "escaped", 94));
                 pos = 71;
               };
           ];
         pos = 7;
         body =
           A.SeqExp
             [
               ( A.CallExp
                   { func = Symbol.to_symbol "getN"; args = []; pos = 121 },
                 121 );
             ];
       })

let%test_unit "unescaped_param_func_dec" =
  [%test_eq: A.exp]
    (let exp =
       Parser.parse_string
         {|
           let
             function identity(i: int): int = i
           in
             identity(5)
           end 
         |}
     in
     FindEscape.find_escape exp;
     exp)
    (A.LetExp
       {
         decs =
           [
             A.FunctionDec
               {
                 name = Symbol.to_symbol "identity";
                 params =
                   [
                     {
                       name = Symbol.to_symbol "i";
                       escape = ref false;
                       typ = Symbol.to_symbol "int";
                       pos = 47;
                     };
                   ];
                 result = Some (Symbol.to_symbol "int", 56);
                 body = A.VarExp (A.SimpleVar (Symbol.to_symbol "i", 62));
                 pos = 29;
               };
           ];
         pos = 12;
         body =
           A.SeqExp
             [
               ( A.CallExp
                   {
                     func = Symbol.to_symbol "identity";
                     args = [ A.IntExp 5 ];
                     pos = 91;
                   },
                 91 );
             ];
       })

let%test_unit "escaped_param_func_dec" =
  [%test_eq: A.exp]
    (let exp =
       Parser.parse_string
         {|
           let
             function identity(i: int): int =
               let
                 function identity2(): int = i
               in
                 identity2()
               end
           in
             identity(5)
           end
           |}
     in
     FindEscape.find_escape exp;
     exp)
    (A.LetExp
       {
         decs =
           [
             A.FunctionDec
               {
                 name = Symbol.to_symbol "identity";
                 params =
                   [
                     {
                       name = Symbol.to_symbol "i";
                       escape = ref true;
                       typ = Symbol.to_symbol "int";
                       pos = 47;
                     };
                   ];
                 result = Some (Symbol.to_symbol "int", 56);
                 body =
                   A.LetExp
                     {
                       decs =
                         [
                           A.FunctionDec
                             {
                               name = Symbol.to_symbol "identity2";
                               params = [];
                               result = Some (Symbol.to_symbol "int", 120);
                               body =
                                 A.VarExp
                                   (A.SimpleVar (Symbol.to_symbol "i", 126));
                               pos = 98;
                             };
                         ];
                       pos = 77;
                       body =
                         A.SeqExp
                           [
                             ( A.CallExp
                                 {
                                   func = Symbol.to_symbol "identity2";
                                   args = [];
                                   pos = 163;
                                 },
                               163 );
                           ];
                     };
                 pos = 29;
               };
           ];
         pos = 12;
         body =
           A.SeqExp
             [
               ( A.CallExp
                   {
                     func = Symbol.to_symbol "identity";
                     args = [ A.IntExp 5 ];
                     pos = 221;
                   },
                 221 );
             ];
       })

let%test_unit "simple_for_loop_no_escape" =
  [%test_eq: A.exp]
    (let exp = Parser.parse_string "for i := 0 to 10 do print(i)" in
     FindEscape.find_escape exp;
     exp)
    (A.ForExp
       {
         var = Symbol.to_symbol "i";
         escape = ref false;
         lo = A.IntExp 0;
         hi = A.IntExp 10;
         pos = 0;
         body =
           A.CallExp
             {
               func = Symbol.to_symbol "print";
               args = [ A.VarExp (A.SimpleVar (Symbol.to_symbol "i", 26)) ];
               pos = 20;
             };
       })

let%test_unit "for_loop_with_escape" =
  [%test_eq: A.exp]
    (let exp =
       Parser.parse_string
         {|
      for i := 0 to 10 do
        let
          function getI(): int = i
        in
          (getI(); nil)
        end
    |}
     in
     FindEscape.find_escape exp;
     exp)
    (A.ForExp
       {
         var = Symbol.to_symbol "i";
         escape = ref true;
         lo = A.IntExp 0;
         hi = A.IntExp 10;
         pos = 7;
         body =
           A.LetExp
             {
               decs =
                 [
                   A.FunctionDec
                     {
                       name = Symbol.to_symbol "getI";
                       params = [];
                       result = Some (Symbol.to_symbol "int", 66);
                       body = A.VarExp (A.SimpleVar (Symbol.to_symbol "i", 72));
                       pos = 49;
                     };
                 ];
               pos = 35;
               body =
                 A.SeqExp
                   [
                     ( A.SeqExp
                         [
                           ( A.CallExp
                               {
                                 func = Symbol.to_symbol "getI";
                                 args = [];
                                 pos = 96;
                               },
                             96 );
                           (A.NilExp 104, 96);
                         ],
                       95 );
                   ];
             };
       })
