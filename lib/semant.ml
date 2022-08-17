open Env
open Symbol
open Types
open Absyn
open Translate
open Errormsg
module A = Absyn
module E = Env
module S = Symbol

module type SEMANT = sig
  type venv
  type tenv
  type expty
  type decty
  type senv (* Scope environment *)

  exception Internal_error

  val transVar : venv * tenv * senv * A.var -> expty
  val transExp : venv * tenv * senv * A.exp -> expty
  val transDecs : venv * tenv * senv * A.dec list -> decty
  val transTy : tenv * A.ty -> Types.ty
  val transProg : A.exp -> unit
end

module Semant : SEMANT = struct
  type venv = Env.enventry Symbol.t
  type tenv = Types.ty Symbol.t
  type expty = { exp : Translate.exp; ty : Types.ty }
  type decty = { venv : venv; tenv : tenv }
  type senv = { in_loop : bool }

  exception Internal_error

  let check_type (required_type, { ty; _ }, pos, msg) =
    if ty != required_type then ErrorMsg.error_pos pos msg

  let rec actual_ty = function
    | Types.NAME (_, t) -> (
        match !t with Some t' -> actual_ty t' | None -> raise Internal_error)
    | t -> t

  module StringSet = Set.Make (String)

  let base_senv = { in_loop = false }

  let symbol_to_type s tenv pos =
    match Symbol.look (tenv, s) with
    | Some t' -> t'
    | None ->
        ErrorMsg.error_pos pos
          (Printf.sprintf "undefined type %s" (Symbol.name s));
        Types.INT

  let rec transExp (venv, tenv, senv, exp) =
    let rec trexp = function
      | A.NilExp _ -> { exp = (); ty = Types.NIL }
      | A.IntExp _ -> { exp = (); ty = Types.INT }
      | A.StringExp _ -> { exp = (); ty = Types.STRING }
      | A.CallExp { func = id; args; pos } -> (
          match Symbol.look (venv, id) with
          | Some (E.FunEntry { formals; result }) ->
              let args_len = List.length args in
              let formals_len = List.length formals in
              if args_len = formals_len then (
                List.iter argMatchesType (List.combine args formals);
                { exp = (); ty = actual_ty result })
              else (
                ErrorMsg.error_pos pos
                  (Printf.sprintf "%s takes %d argument(s) (%d given)"
                     (S.name id) formals_len args_len);
                { exp = (); ty = actual_ty result })
          | _ ->
              ErrorMsg.error_pos pos
                (Printf.sprintf "undefined function %s" (S.name id));
              { exp = (); ty = Types.INT })
      | A.OpExp { left; oper; right; pos } when A.is_comparison_op oper ->
          let { ty = left_ty; _ } = trexp left in
          let { ty = right_ty; _ } = trexp right in
          (match left_ty with
          | Types.INT | Types.STRING | Types.NIL | Types.RECORD _
          | Types.ARRAY _ ->
              if not (Types.equals (left_ty, right_ty)) then
                ErrorMsg.error_pos pos
                  (Printf.sprintf
                     "both expressions should have matching types, left:%s \
                      right:%s"
                     (Types.to_string left_ty) (Types.to_string right_ty))
          | _ ->
              ErrorMsg.error_pos pos
                "comparison operators are only compatible with INT, STRING, \
                 ARRAY and RECORD types");
          { exp = (); ty = Types.INT }
      | A.OpExp { left; oper; right; _ } when A.is_arithmetic_op oper ->
          check_type
            ( Types.INT,
              trexp left,
              A.exp_pos left,
              "INT arguments required for arithmetic operator" );
          check_type
            ( Types.INT,
              trexp right,
              A.exp_pos right,
              "INT arguments required for arithmetic operator" );
          { exp = (); ty = Types.INT }
      | A.OpExp _ -> raise Internal_error
      | A.RecordExp { fields = input_fields; typ; pos } -> (
          match Symbol.look (tenv, typ) with
          | Some t -> (
              let t' = actual_ty t in
              match t' with
              | Types.RECORD (fields, _) ->
                  (let input_symbols =
                     List.fold_left
                       (fun acc (s, _, _) -> StringSet.add (Symbol.name s) acc)
                       StringSet.empty input_fields
                   in
                   let required_symbols =
                     List.fold_left
                       (fun acc (s, _) -> StringSet.add (Symbol.name s) acc)
                       StringSet.empty fields
                   in
                   if StringSet.equal input_symbols required_symbols then
                     let sorted_fields =
                       List.map
                         (fun (s, t) -> (s, actual_ty t))
                         (List.sort
                            (fun (s1, _) (s2, _) -> Symbol.compare_symbol s1 s2)
                            fields)
                     in
                     let sorted_input_fields =
                       List.map
                         (fun (s, e, _) -> (s, (trexp e).ty))
                         (List.sort
                            (fun (s1, _, _) (s2, _, _) ->
                              Symbol.compare_symbol s1 s2)
                            input_fields)
                     in
                     List.iter2
                       (fun (s1, t1) (_, t2) ->
                         if not (Types.equals (t1, t2)) then
                           ErrorMsg.error_pos pos
                             (Printf.sprintf
                                "type mismatch for field '%s', expected %s \
                                 (got %s)"
                                (Symbol.name s1) (Types.to_string t1)
                                (Types.to_string t2))
                         else ())
                       sorted_fields sorted_input_fields
                   else
                     ErrorMsg.error_pos pos
                       "invalid fields in record initialisation");
                  { exp = (); ty = t' }
              | _ ->
                  ErrorMsg.error_pos pos
                    (Printf.sprintf "invalid record type: %s"
                       (Types.to_string t'));
                  { exp = (); ty = Types.INT })
          | None ->
              ErrorMsg.error_pos pos
                (Printf.sprintf "undefined type %s" (S.name typ));
              { exp = (); ty = Types.INT })
      | SeqExp exps ->
          (* Returns the NIL type for empty sequences *)
          List.fold_left
            (fun _ (e, _) -> trexp e)
            { exp = (); ty = Types.NIL }
            exps
      | AssignExp { var; exp; pos } ->
          let { ty = var_type; _ } = transVar (venv, tenv, senv, var) in
          let { ty = exp_type; _ } = trexp exp in
          if var_type != exp_type then
            ErrorMsg.error_pos pos
              (Printf.sprintf
                 "type mismatch: tried to assign expression of type %s to \
                  variable of type %s"
                 (Types.to_string exp_type) (Types.to_string var_type));
          (* Assignment operation produces no value *)
          { exp = (); ty = Types.NIL }
      | IfExp { test; then'; else'; _ } -> (
          check_type
            ( Types.INT,
              trexp test,
              A.exp_pos test,
              "if statement condition must be an INT" );
          let then_type = trexp then' in
          (* When the else clause is absent, the then clause
             has to return NIL. Otherwise, both clauses have
             to return the same type *)
          match else' with
          | Some e ->
              let else_type = (trexp e).ty in
              if not (Types.equals (then_type.ty, else_type)) then
                ErrorMsg.error_pos (A.exp_pos then')
                  "type mismatch in then and else clause";
              { exp = (); ty = then_type.ty }
          | None ->
              check_type
                ( Types.NIL,
                  then_type,
                  A.exp_pos then',
                  "then clause has to return NIL" );
              { exp = (); ty = Types.NIL })
      | WhileExp { test; body; _ } ->
          check_type
            ( Types.INT,
              trexp test,
              A.exp_pos test,
              "while statement condition must be an INT" );
          let senv' = { in_loop = true } in
          check_type
            ( Types.NIL,
              transExp (venv, tenv, senv', body),
              A.exp_pos body,
              "while statement body must return NIL" );
          { exp = (); ty = Types.NIL }
      | ForExp { var; lo; hi; body; _ } ->
          check_type
            ( Types.INT,
              trexp lo,
              A.exp_pos lo,
              "for loop start variable must be an INT" );
          check_type
            ( Types.INT,
              trexp hi,
              A.exp_pos hi,
              "for loop end variable must be an INT" );
          let venv' = Symbol.enter (venv, var, E.VarEntry { ty = Types.INT }) in
          let senv' = { in_loop = true } in
          check_type
            ( Types.NIL,
              transExp (venv', tenv, senv', body),
              A.exp_pos body,
              "for loop body must return NIL" );
          { exp = (); ty = Types.NIL }
      | BreakExp pos ->
          if not senv.in_loop then
            ErrorMsg.error_pos pos
              "encountered break statement when not in loop";
          { exp = (); ty = Types.NIL }
      | ArrayExp { typ; size; init; pos } -> (
          check_type
            (Types.INT, trexp size, A.exp_pos size, "array size must be INT");
          match Symbol.look (tenv, typ) with
          | Some ty -> (
              match actual_ty ty with
              | Types.ARRAY (t, _) as ty' ->
                  check_type
                    ( t,
                      trexp init,
                      A.exp_pos init,
                      Printf.sprintf
                        "array initial value expected to be of type %s"
                        (Types.to_string t) );
                  { exp = (); ty = ty' }
              | _ -> { exp = (); ty = Types.INT })
          | _ ->
              ErrorMsg.error_pos pos
                (Printf.sprintf "undefined array type %s" (S.name typ));
              { exp = (); ty = Types.ARRAY (Types.INT, ref ()) })
      | LetExp { decs; body; _ } ->
          let { venv; tenv } = transDecs (venv, tenv, senv, decs) in
          transExp (venv, tenv, senv, body)
      | VarExp var -> transVar (venv, tenv, senv, var)
    (* Used to check that function arguments match the call signature *)
    and argMatchesType (exp, ty) =
      let { ty = exp_ty; _ } = trexp exp in
      let ty' = actual_ty ty in
      if exp_ty != ty' then
        ErrorMsg.error_pos (Absyn.exp_pos exp)
          (Printf.sprintf "Expected argument of type %s got %s instead"
             (Types.to_string ty') (Types.to_string exp_ty))
    in
    trexp exp

  and transVar (venv, tenv, senv, var) =
    let rec trvar = function
      | A.SimpleVar (id, pos) -> (
          match Symbol.look (venv, id) with
          | Some (E.VarEntry { ty }) -> { exp = (); ty = actual_ty ty }
          | _ ->
              ErrorMsg.error_pos pos
                (Printf.sprintf "undefined variable %s" (S.name id));
              { exp = (); ty = Types.INT })
      | A.FieldVar (var, sym, pos) -> (
          match actual_ty (trvar var).ty with
          | Types.RECORD (fields, _) ->
              let rec find_field = function
                | [] ->
                    ErrorMsg.error_pos pos
                      "attempted to access field that does not exist in record";
                    Types.INT
                | (s, t) :: _ when s = sym -> actual_ty t
                | _ :: rest -> find_field rest
              in
              { exp = (); ty = find_field fields }
          | _ ->
              ErrorMsg.error_pos pos
                "attempted to access field on a non-record type";
              { exp = (); ty = Types.INT })
      | A.SubscriptVar (var, exp, pos) -> (
          check_type
            ( Types.INT,
              transExp (venv, tenv, senv, exp),
              pos,
              "non-INT type cannot be used as index into array" );
          match actual_ty (trvar var).ty with
          | Types.ARRAY (ty, _) -> { exp = (); ty }
          | _ ->
              ErrorMsg.error_pos pos "attempted to index into a non-array type";
              { exp = (); ty = Types.INT })
    in
    trvar var

  (* Adds dummy header to the type environment
     with an empty body for type and function
     declarations *)
  and extractHeader (venv, tenv, dec) =
    let sym_to_type (sym, pos) =
      match Symbol.look (tenv, sym) with
      | Some t' -> t'
      | _ ->
          ErrorMsg.error_pos pos
            (Printf.sprintf "undefined type: %s" (Symbol.name sym));
          Types.INT
    in
    match dec with
    | A.FunctionDec { name; params; result; _ } ->
        let ret_type =
          match result with
          | Some (s, pos) -> sym_to_type (s, pos)
          (* If no return type is specified, then this is a procedure *)
          | _ -> Types.NIL
        in

        {
          venv =
            S.enter
              ( venv,
                name,
                E.FunEntry
                  {
                    formals =
                      List.map
                        (fun (param : A.field) ->
                          sym_to_type (param.typ, param.pos))
                        params;
                    result = ret_type;
                  } );
          tenv;
        }
    | A.TypeDec { name; _ } ->
        (* Add in placeholder type *)
        { venv; tenv = S.enter (tenv, name, Types.NAME (name, ref None)) }
    (* Do nothing for other types *)
    | _ -> { venv; tenv }

  (* Actually process the bodies of type/function
     declarations and variable definitions *)
  and processBody (venv, tenv, senv, dec) =
    match dec with
    | A.TypeDec { name; ty; _ } ->
        let t = transTy (tenv, ty) in
        (match Symbol.look (tenv, name) with
        | Some (Types.NAME (_, t')) -> t' := Some t
        | _ -> raise Internal_error);
        { venv; tenv }
    | A.FunctionDec { name; params; body; _ } ->
        let param_to_type (param : A.field) =
          match Symbol.look (tenv, param.typ) with
          | Some t' -> actual_ty t'
          | _ -> Types.INT
        in
        (* Env with function parameters *)
        let venv' =
          let do_param env param =
            let ty = param_to_type param in
            Symbol.enter (env, param.name, E.VarEntry { ty })
          in
          List.fold_left do_param venv params
        in
        let body_type = transExp (venv', tenv, senv, body) in
        (match Symbol.look (venv, name) with
        | Some (FunEntry { result; _ }) ->
            let res_type = actual_ty result in
            if not (Types.equals (res_type, body_type.ty)) then
              ErrorMsg.error_pos (A.exp_pos body)
                (Printf.sprintf
                   "function return type does not match body, required %s got \
                    %s instead"
                   (Types.to_string result)
                   (Types.to_string body_type.ty))
        | _ -> raise Internal_error);
        { venv; tenv }
    | A.VarDec { name; typ = None; init; _ } ->
        let { ty; _ } = transExp (venv, tenv, senv, init) in
        { venv = S.enter (venv, name, E.VarEntry { ty }); tenv }
    | A.VarDec { name; typ = Some (t, t_pos); init; _ } ->
        let exp_ty = transExp (venv, tenv, senv, init) in
        (match Symbol.look (tenv, t) with
        | Some t' ->
            if actual_ty t' != exp_ty.ty then
              ErrorMsg.error_pos t_pos
                (Printf.sprintf "type mismatch, expected %s but got %s instead"
                   (Symbol.name t)
                   (Types.to_string exp_ty.ty))
        | None ->
            ErrorMsg.error_pos t_pos
              (Printf.sprintf "undefined type: %s" (Symbol.name t)));
        { venv = S.enter (venv, name, E.VarEntry { ty = exp_ty.ty }); tenv }

  (* Two passes
     1. Process headers of type/function declarations and
        set up placeholders.
     2. Process bodies of type/function declarations and
        replace placeholders with actual bodies. Also handles
        variable definitions (we do it here as these
        definitions might use recursive types and we need to
        call actual_ty on them)*)
  and transDecs (venv, tenv, senv, decs) =
    let { venv = venv'; tenv = tenv' } =
      List.fold_left
        (fun { venv = v; tenv = t } dec -> extractHeader (v, t, dec))
        { venv; tenv } decs
    in
    List.fold_left
      (fun { venv = v; tenv = t } dec -> processBody (v, t, senv, dec))
      { venv = venv'; tenv = tenv' }
      decs

  (* Translates Absyn.ty to Types.ty *)
  and transTy (tenv, ty) =
    match ty with
    | NameTy (t, pos) -> (
        match Symbol.look (tenv, t) with
        | Some t' -> Types.NAME (t, ref (Some t'))
        | None ->
            ErrorMsg.error_pos pos
              (Printf.sprintf "undefined type: %s" (Symbol.name t));
            Types.NAME (t, ref None))
    | RecordTy fields ->
        let field_to_sym_type acc (field : A.field) =
          acc @ [ (field.name, symbol_to_type field.typ tenv field.pos) ]
        in
        let res = List.fold_left field_to_sym_type [] fields in
        Types.RECORD (res, ref ())
    | ArrayTy (t, pos) -> (
        match Symbol.look (tenv, t) with
        | Some t' -> Types.ARRAY (actual_ty t', ref ())
        | None ->
            ErrorMsg.error_pos pos
              (Printf.sprintf "undefined type: %s" (Symbol.name t));
            Types.ARRAY (Types.INT, ref ()))

  let transProg exp =
    ignore (transExp (E.base_venv, E.base_tenv, base_senv, exp))
end

open Base
open Errormsg

let%test_unit "successfully_run_semant_on_test_files" =
  let do_file filename =
    ignore (List.map ~f:Semant.transProg (Parser.parse_file filename))
  in
  let res =
    let test_dir = "../../../tests/" in
    Caml.Sys.readdir test_dir |> Array.to_list
    |> List.filter ~f:(fun x -> String.(Caml.Filename.extension x = ".tig"))
    |> List.map ~f:(fun fname ->
           do_file (test_dir ^ fname);
           !ErrorMsg.anyErrors)
  in
  [%test_eq: bool list] res (List.map ~f:(fun _ -> false) res)

let%test_unit "legal_break_statement_in_for_loop" =
  let input_string = "let in for i := 0 to 10 do break end" in
  ignore (List.map ~f:Semant.transProg (Parser.parse_string input_string));
  [%test_eq: bool] !ErrorMsg.anyErrors false

let%test_unit "legal_break_statement_in_while_loop" =
  let input_string = "let in while 1 do break end" in
  ignore (List.map ~f:Semant.transProg (Parser.parse_string input_string));
  [%test_eq: bool] !ErrorMsg.anyErrors false

let%expect_test "illegal_break_statement_outside_of_for_loop" =
  let input_string = "let in break end" in
  ignore (List.map ~f:Semant.transProg (Parser.parse_string input_string));
  [%expect {| :1.7: encountered break statement when not in loop |}]
