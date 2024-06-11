open Base
open Env
open Symbol
open Types
open Absyn
open Translate
open Temp
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
  exception Semantic_error

  val transVar : venv * tenv * senv * Translate.level * A.var -> expty
  val transExp : venv * tenv * senv * Translate.level * A.exp -> expty
  val transDecs : venv * tenv * senv * Translate.level * A.dec list -> decty
  val transTy : tenv * A.ty -> Types.ty
  val transProg : A.exp -> Translate.frag list
end

module Semant : SEMANT = struct
  type venv = Env.enventry Symbol.tbl (* Value environment *)
  type tenv = Types.ty Symbol.tbl (* Type environment *)
  type expty = { exp : Translate.exp; ty : Types.ty }

  type senv = {
    break : Temp.label option (* Label of the closest enclosing loop *);
  }

  type decty = {
    venv : venv;
    tenv : tenv;
    senv : senv;
    inits : Translate.exp list; (* Result of evaluating each declaration *)
  }

  exception Internal_error
  exception Semantic_error

  let check_type (required_type, { ty; _ }, pos, msg) =
    if not @@ Types.equals (ty, required_type) then ErrorMsg.error_pos pos msg

  let rec actual_ty = function
    | Types.NAME (_, t) -> (
        match !t with Some t' -> actual_ty t' | None -> raise Internal_error)
    | t -> t

  module StringSet = Stdlib.Set.Make (String)

  let base_senv = { break = None }

  let symbol_to_type s tenv pos =
    match Symbol.look (tenv, s) with
    | Some t' -> t'
    | None ->
        ErrorMsg.error_pos pos
          (Printf.sprintf "undefined type %s" (Symbol.name s));
        Types.INT

  let rec transExp (venv, tenv, senv, level, exp) =
    let rec trexp = function
      | A.NilExp _ -> { exp = Translate.intExp 0; ty = Types.NIL }
      | A.IntExp i -> { exp = Translate.intExp i; ty = Types.INT }
      | A.StringExp (s, _) -> { exp = Translate.stringExp s; ty = Types.STRING }
      | A.CallExp { func = id; args; pos } -> (
          match Symbol.look (venv, id) with
          | Some (E.FunEntry { formals; result; label; level = fn_level; _ }) ->
              let args_len = List.length args in
              let formals_len = List.length formals in
              if args_len = formals_len then
                let processed_args =
                  List.map ~f:processArg (List.zip_exn args formals)
                in
                {
                  exp =
                    Translate.callExp (label, processed_args, fn_level, level);
                  ty = actual_ty result;
                }
              else (
                ErrorMsg.error_pos pos
                  (Printf.sprintf "%s takes %d argument(s) (%d given)"
                     (S.name id) formals_len args_len);
                { exp = Translate.default_exp; ty = actual_ty result })
          | _ ->
              ErrorMsg.error_pos pos
                (Printf.sprintf "undefined function %s" (S.name id));
              { exp = Translate.default_exp; ty = Types.INT })
      | A.OpExp { left; oper; right; pos } when A.is_comparison_op oper ->
          let { ty = left_ty; exp = left_exp } = trexp left in
          let { ty = right_ty; exp = right_exp } = trexp right in
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
          {
            exp = Translate.comparisonOperation (left_exp, oper, right_exp);
            ty = Types.INT;
          }
      | A.OpExp { left; oper; right; _ } when A.is_arithmetic_op oper ->
          let left_expty = trexp left in
          let right_expty = trexp right in
          check_type
            ( Types.INT,
              left_expty,
              A.exp_pos left,
              "INT arguments required for arithmetic operator" );
          check_type
            ( Types.INT,
              right_expty,
              A.exp_pos right,
              "INT arguments required for arithmetic operator" );
          {
            exp =
              Translate.arithmeticOperation
                (left_expty.exp, oper, right_expty.exp);
            ty = Types.INT;
          }
      | A.OpExp _ -> raise Internal_error
      | A.RecordExp { fields = input_fields; typ; pos } -> (
          match Symbol.look (tenv, typ) with
          | Some t -> (
              let record_type = actual_ty t in
              match record_type with
              | Types.RECORD (fields, _) ->
                  if not (List.length input_fields = List.length fields) then (
                    ErrorMsg.error_pos pos
                      (Printf.sprintf
                         "record initialisation expected %d fields (got %d)"
                         (List.length fields) (List.length input_fields));
                    { exp = Translate.default_exp; ty = record_type })
                  else
                    let input_field_map =
                      List.fold
                        ~f:(fun tbl (sym, exp, _) -> S.enter (tbl, sym, exp))
                        ~init:S.empty input_fields
                    in
                    let input_field_exps =
                      List.fold
                        ~f:(fun acc (sym, ty) ->
                          match S.look (input_field_map, sym) with
                          | Some e ->
                              let { exp; ty = ty' } = trexp e in
                              let ty = actual_ty ty in
                              let ty' = actual_ty ty' in
                              if not (Types.equals (ty, ty')) then
                                ErrorMsg.error_pos pos
                                  (Printf.sprintf
                                     "type mismatch for field '%s', expected \
                                      %s (got %s)"
                                     (Symbol.name sym) (Types.to_string ty)
                                     (Types.to_string ty'));
                              exp :: acc
                          | None ->
                              ErrorMsg.error_pos pos
                                (Printf.sprintf
                                   "missing field '%s' in record initialisation"
                                   (S.name sym));
                              acc)
                        ~init:[] fields
                      |> List.rev
                    in
                    {
                      exp = Translate.recordExp input_field_exps;
                      ty = record_type;
                    }
              | _ ->
                  ErrorMsg.error_pos pos
                    (Printf.sprintf "invalid record type: %s"
                       (Types.to_string record_type));
                  { exp = Translate.default_exp; ty = Types.INT })
          | None ->
              ErrorMsg.error_pos pos
                (Printf.sprintf "undefined type %s" (S.name typ));
              { exp = Translate.default_exp; ty = Types.INT })
      | SeqExp exps ->
          (* Returns the NIL type for empty sequences *)
          let exps', ty =
            List.fold_left
              ~f:(fun (l, _) (e, _) ->
                let { exp; ty } = trexp e in
                (l @ [ exp ], ty))
              ~init:([], Types.NIL) exps
          in
          { exp = Translate.seqExp exps'; ty }
      | AssignExp { var; exp; pos } ->
          let { ty = var_type; exp = var_exp } =
            transVar (venv, tenv, senv, level, var)
          in
          let { ty = exp_type; exp = exp_exp } = trexp exp in
          if not (Types.equals (actual_ty var_type, actual_ty exp_type)) then
            ErrorMsg.error_pos pos
              (Printf.sprintf
                 "type mismatch: tried to assign expression of type %s to \
                  variable of type %s"
                 (Types.to_string exp_type) (Types.to_string var_type));
          (* Assignment operation produces no value *)
          { exp = Translate.assignExp (var_exp, exp_exp); ty = Types.NIL }
      | IfExp { test; then'; else'; _ } -> (
          let test_expty = trexp test in
          check_type
            ( Types.INT,
              test_expty,
              A.exp_pos test,
              "if statement condition must be an INT" );
          let then_expty = trexp then' in
          (* When the else clause is absent, the then clause
             has to return NIL. Otherwise, both clauses have
             to return the same type *)
          match else' with
          | Some e ->
              let else_expty = trexp e in
              if not (Types.equals (then_expty.ty, else_expty.ty)) then
                ErrorMsg.error_pos (A.exp_pos then')
                  "type mismatch in then and else clause";
              {
                exp =
                  Translate.ifThenElse
                    (test_expty.exp, then_expty.exp, else_expty.exp);
                ty = then_expty.ty;
              }
          | None ->
              check_type
                ( Types.NIL,
                  then_expty,
                  A.exp_pos then',
                  "then clause has to return NIL" );
              {
                exp = Translate.ifThen (test_expty.exp, then_expty.exp);
                ty = Types.NIL;
              })
      | WhileExp { test; body; _ } ->
          let test_expty = trexp test in
          check_type
            ( Types.INT,
              test_expty,
              A.exp_pos test,
              "while statement condition must be an INT" );
          let end_label = Temp.new_label () in
          let senv' = { break = Some end_label } in
          let body_expty = transExp (venv, tenv, senv', level, body) in
          check_type
            ( Types.NIL,
              body_expty,
              A.exp_pos body,
              "while statement body must return NIL" );
          {
            exp = Translate.whileExp (test_expty.exp, body_expty.exp, end_label);
            ty = Types.NIL;
          }
      | ForExp { var; lo; hi; body; escape; _ } ->
          let lo_expty = trexp lo in
          let hi_expty = trexp hi in
          check_type
            ( Types.INT,
              lo_expty,
              A.exp_pos lo,
              "for loop start variable must be an INT" );
          check_type
            ( Types.INT,
              hi_expty,
              A.exp_pos hi,
              "for loop end variable must be an INT" );
          let loop_counter_access = Translate.alloc_local level !escape in
          let venv' =
            Symbol.enter
              ( venv,
                var,
                E.VarEntry { ty = Types.INT; access = loop_counter_access } )
          in
          let end_label = Temp.new_label () in
          let senv' = { break = Some end_label } in
          let body_expty = transExp (venv', tenv, senv', level, body) in
          check_type
            ( Types.NIL,
              body_expty,
              A.exp_pos body,
              "for loop body must return NIL" );
          let loop_counter_exp =
            Translate.simpleVar (loop_counter_access, level)
          in
          {
            exp =
              Translate.forExp
                ( loop_counter_exp,
                  lo_expty.exp,
                  hi_expty.exp,
                  body_expty.exp,
                  end_label );
            ty = Types.NIL;
          }
      | BreakExp pos -> (
          match senv.break with
          | Some end_label ->
              { exp = Translate.breakExp end_label; ty = Types.NIL }
          | _ ->
              ErrorMsg.error_pos pos
                "encountered break statement when not in loop";
              { exp = Translate.default_exp; ty = Types.NIL })
      | ArrayExp { typ; size; init; pos } -> (
          let size_expty = trexp size in
          check_type
            (Types.INT, size_expty, A.exp_pos size, "array size must be INT");
          match Symbol.look (tenv, typ) with
          | Some ty -> (
              match actual_ty ty with
              | Types.ARRAY (t, _) as ty' ->
                  let init_expty = trexp init in
                  check_type
                    ( t,
                      init_expty,
                      A.exp_pos init,
                      Printf.sprintf
                        "array initial value expected to be of type %s"
                        (Types.to_string t) );
                  {
                    exp = Translate.arrayExp (size_expty.exp, init_expty.exp);
                    ty = ty';
                  }
              | _ -> { exp = Translate.default_exp; ty = Types.INT })
          | _ ->
              ErrorMsg.error_pos pos
                (Printf.sprintf "undefined array type %s" (S.name typ));
              {
                exp = Translate.default_exp;
                ty = Types.ARRAY (Types.INT, ref ());
              })
      | LetExp { decs; body; _ } ->
          let { venv; tenv; inits; senv } =
            transDecs (venv, tenv, senv, level, decs)
          in
          let res = transExp (venv, tenv, senv, level, body) in
          { res with exp = Translate.seqExp (inits @ [ res.exp ]) }
      | VarExp var -> transVar (venv, tenv, senv, level, var)
    (* Used to check that function arguments match the call signature *)
    and processArg (exp, ty) =
      let { ty = exp_ty; exp = exp' } = trexp exp in
      let ty' = actual_ty ty in
      if not (Types.equals (exp_ty, ty')) then
        ErrorMsg.error_pos (Absyn.exp_pos exp)
          (Printf.sprintf "Expected argument of type %s got %s instead"
             (Types.to_string ty') (Types.to_string exp_ty));
      exp'
    in
    trexp exp

  and transVar (venv, tenv, senv, level, var) =
    let rec trvar = function
      | A.SimpleVar (id, pos) -> (
          match Symbol.look (venv, id) with
          | Some (E.VarEntry { ty; access }) ->
              { exp = Translate.simpleVar (access, level); ty = actual_ty ty }
          | _ ->
              ErrorMsg.error_pos pos
                (Printf.sprintf "undefined variable %s" (S.name id));
              { exp = Translate.default_exp; ty = Types.INT })
      | A.FieldVar (var, sym, pos) -> (
          let var_res = trvar var in
          match actual_ty var_res.ty with
          | Types.RECORD (fields, _) ->
              let rec find_field fields idx =
                match fields with
                | [] ->
                    ErrorMsg.error_pos pos
                      "attempted to access field that does not exist in record";
                    (Types.INT, 0)
                | (s, t) :: _ when S.equal s sym -> (actual_ty t, idx)
                | _ :: rest -> find_field rest (idx + 1)
              in
              let t, field_index = find_field fields 0 in
              { exp = Translate.fieldVar (var_res.exp, field_index); ty = t }
          | _ ->
              ErrorMsg.error_pos pos
                "attempted to access field on a non-record type";
              { exp = Translate.default_exp; ty = Types.INT })
      | A.SubscriptVar (var, exp, pos) -> (
          let index_res = transExp (venv, tenv, senv, level, exp) in
          let var_res = trvar var in
          check_type
            ( Types.INT,
              index_res,
              pos,
              "non-INT type cannot be used as index into array" );
          match actual_ty var_res.ty with
          | Types.ARRAY (ty, _) ->
              { exp = Translate.subscriptVar (var_res.exp, index_res.exp); ty }
          | _ ->
              ErrorMsg.error_pos pos "attempted to index into a non-array type";
              { exp = Translate.default_exp; ty = Types.INT })
    in
    trvar var

  (* Adds dummy header to the type environment with an empty body for type and
     function declarations. `declared_fns` and `declared_types` keep track
     of the stuff defined in this scope to avoid duplicated names. *)
  and extractHeader (venv, tenv, declared_fns, declared_types, level, dec) =
    let sym_to_type (sym, pos) =
      match Symbol.look (tenv, sym) with
      | Some t' -> t'
      | _ ->
          ErrorMsg.error_pos pos
            (Printf.sprintf "undefined type: %s" (Symbol.name sym));
          Types.INT
    in
    match dec with
    | A.FunctionDec { name; params; result; pos; _ } ->
        if List.mem declared_fns name ~equal:Symbol.equal then (
          ErrorMsg.error_pos pos
            (Printf.sprintf "duplicate function definition '%s' in local scope"
               (Symbol.name name));
          (venv, tenv, declared_fns, declared_types))
        else
          let ret_type =
            match result with
            | Some (s, pos) -> sym_to_type (s, pos)
            (* If no return type is specified, then this is a procedure *)
            | _ -> Types.NIL
          in
          let label = Temp.label_from_suffix @@ Symbol.name name in
          let level' =
            Translate.new_level ~parent:level ~name:label
              ~formals:(List.map ~f:(fun (f : A.field) -> !(f.escape)) params)
              ~static:true
          in
          ( S.enter
              ( venv,
                name,
                E.FunEntry
                  {
                    formals =
                      List.map
                        ~f:(fun (param : A.field) ->
                          sym_to_type (param.typ, param.pos))
                        params;
                    result = ret_type;
                    label;
                    level = level';
                  } ),
            tenv,
            name :: declared_fns,
            declared_types )
    | A.TypeDec { name; pos; _ } ->
        if List.mem declared_types name ~equal:Symbol.equal then (
          ErrorMsg.error_pos pos
            (Printf.sprintf "duplicate type definition '%s' in local scope"
               (Symbol.name name));
          (venv, tenv, declared_fns, declared_types))
        else
          (* Add in placeholder type *)
          ( venv,
            S.enter (tenv, name, Types.NAME (name, ref None)),
            declared_fns,
            name :: declared_types )
    (* Do nothing for other types *)
    | _ -> (venv, tenv, declared_fns, declared_types)

  (* Actually process the bodies of type/function
     declarations and variable definitions *)
  and processBody (venv, tenv, senv, level, dec) =
    match dec with
    | A.TypeDec { name; ty; _ } ->
        let t = transTy (tenv, ty) in
        (match Symbol.look (tenv, name) with
        | Some (Types.NAME (_, t')) -> t' := Some t
        | _ -> raise Internal_error);
        { venv; tenv; senv; inits = [] }
    | A.FunctionDec { name; params; body; _ } ->
        let param_to_type (param : A.field) =
          match Symbol.look (tenv, param.typ) with
          | Some t' -> actual_ty t'
          | _ -> Types.INT
        in
        (match Symbol.look (venv, name) with
        | Some (FunEntry { result; level = level'; _ }) ->
            (* Env with function parameters *)
            let venv' =
              let do_param env param access =
                let ty = param_to_type param in
                Symbol.enter (env, param.name, E.VarEntry { ty; access })
              in
              List.fold2_exn ~f:do_param ~init:venv params
                (Translate.formals level')
            in
            let body_expty = transExp (venv', tenv, senv, level', body) in
            let res_type = actual_ty result in
            if not (Types.equals (res_type, body_expty.ty)) then
              ErrorMsg.error_pos (A.exp_pos body)
                (Printf.sprintf
                   "function return type does not match body, required %s got \
                    %s instead"
                   (Types.to_string result)
                   (Types.to_string body_expty.ty));
            Translate.procEntryExit (body_expty.exp, level')
        | _ -> raise Internal_error);
        { venv; tenv; senv; inits = [] }
    | A.VarDec { name; typ = None; init; escape; _ } ->
        let init_expty = transExp (venv, tenv, senv, level, init) in
        let var_access = Translate.alloc_local level !escape in
        {
          venv =
            S.enter
              ( venv,
                name,
                E.VarEntry { ty = init_expty.ty; access = var_access } );
          tenv;
          senv;
          inits =
            [
              Translate.assignExp
                (Translate.simpleVar (var_access, level), init_expty.exp);
            ];
        }
    | A.VarDec { name; typ = Some (t, t_pos); init; escape; _ } ->
        let { exp = init_exp; ty = init_ty } =
          transExp (venv, tenv, senv, level, init)
        in
        let init_ty = actual_ty init_ty in
        (match Symbol.look (tenv, t) with
        | Some t' ->
            if not @@ Types.equals (actual_ty t', init_ty) then
              ErrorMsg.error_pos t_pos
                (Printf.sprintf "type mismatch, expected %s but got %s instead"
                   (Symbol.name t) (Types.to_string init_ty))
        | None ->
            ErrorMsg.error_pos t_pos
              (Printf.sprintf "undefined type: %s" (Symbol.name t)));
        let var_access = Translate.alloc_local level !escape in
        {
          venv =
            S.enter
              (venv, name, E.VarEntry { ty = init_ty; access = var_access });
          tenv;
          senv;
          inits =
            [
              Translate.assignExp
                (Translate.simpleVar (var_access, level), init_exp);
            ];
        }

  (* Two passes
     1. Process headers of type/function declarations and set up placeholders.
     2. Process bodies of type/function declarations and replace placeholders
        with actual bodies. Also handles variable definitions (we do it here as
        these definitions might use recursive types and we need to call actual_ty
        on them) *)
  and transDecs (venv, tenv, senv, level, decs) =
    let venv, tenv, _, _ =
      List.fold_left
        ~f:(fun (venv, tenv, declared_fns, declared_types) dec ->
          extractHeader (venv, tenv, declared_fns, declared_types, level, dec))
        ~init:(venv, tenv, [], []) decs
    in
    let res =
      List.fold_left
        ~f:(fun { venv = v; tenv = t; senv = s; inits } dec ->
          let res = processBody (v, t, s, level, dec) in
          { res with inits = List.rev res.inits @ inits })
        ~init:{ venv; tenv; inits = []; senv }
        decs
    in
    { res with inits = List.rev res.inits }

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
        let res = List.fold_left ~f:field_to_sym_type ~init:[] fields in
        Types.RECORD (res, ref ())
    | ArrayTy (t, pos) -> (
        match Symbol.look (tenv, t) with
        | Some t' -> Types.ARRAY (actual_ty t', ref ())
        | None ->
            ErrorMsg.error_pos pos
              (Printf.sprintf "undefined type: %s" (Symbol.name t));
            Types.ARRAY (Types.INT, ref ()))

  let transProg exp =
    Temp.reset ();
    Translate.init ();
    let main_level =
      Translate.new_level
        ~name:(Temp.named_label "tigermain")
        ~parent:Translate.outermost ~formals:[] ~static:false
    in
    let res_expty =
      transExp (E.base_venv, E.base_tenv, base_senv, main_level, exp)
    in
    Translate.procEntryExit (res_expty.exp, main_level);
    if !ErrorMsg.anyErrors then raise Semantic_error else Translate.getResult ()
end

(* Tests *)

open Base
open Errormsg

exception Missing_exception

let%test_unit "successfully_run_semant_on_test_files" =
  let do_file filename =
    ignore (Semant.transProg (Parser.parse_file filename))
  in
  let res =
    let test_dir = "../../../tests/" in
    Stdlib.Sys.readdir test_dir
    |> Array.to_list
    |> List.filter ~f:(fun x -> String.(Stdlib.Filename.extension x = ".tig"))
    |> List.map ~f:(fun fname ->
           do_file (test_dir ^ fname);
           !ErrorMsg.anyErrors)
  in
  [%test_eq: bool list] res (List.map ~f:(fun _ -> false) res)

let%test_unit "legal_break_statement_in_for_loop" =
  let input_string = "let in for i := 0 to 10 do break end" in
  ignore (Semant.transProg (Parser.parse_string input_string));
  [%test_eq: bool] !ErrorMsg.anyErrors false

let%test_unit "legal_break_statement_in_while_loop" =
  let input_string = "let in while 1 do break end" in
  ignore (Semant.transProg (Parser.parse_string input_string));
  [%test_eq: bool] !ErrorMsg.anyErrors false

let%expect_test "illegal_break_statement_outside_of_for_loop" =
  let input_string = "let in break end" in
  try
    ignore (Semant.transProg (Parser.parse_string input_string));
    raise Missing_exception
  with Semant.Semantic_error ->
    ();
    [%expect {| :1.7: encountered break statement when not in loop |}]
