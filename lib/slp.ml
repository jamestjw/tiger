open! Base

type id = string
type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

(* Returns the maximum number of arguments of any print
   statement within any subexpression of a given statement *)
let rec maxargs s =
  match s with
  | CompoundStm (stm1, stm2) -> Int.max (maxargs stm1) (maxargs stm2)
  | AssignStm (_, exp) -> maxargs_exp exp
  (* Might be more nested expressions *)
  | PrintStm exps -> Int.max (List.length exps) (maxargs_explist exps)

and maxargs_exp e = match e with EseqExp (stm, _) -> maxargs stm | _ -> 0

and maxargs_explist exps =
  match exps with
  | exp :: rest -> Int.max (maxargs_exp exp) (maxargs_explist rest)
  | [] -> 0

type table = (id * int) list

let updateTable name value t : table = (name, value) :: t

let rec lookupTable = function
  | name, (id, v) :: _ when String.(name = id) -> Some v
  | name, (_, _) :: rest -> lookupTable (name, rest)
  | _, [] -> None

exception UndefinedVariable of string

let rec interp s =
  let t = [] in
  interpStm s t

and interpStm s t =
  match s with
  | CompoundStm (stm1, stm2) -> interpStm stm2 (interpStm stm1 t)
  | AssignStm (id, exp) ->
      let v, t' = interpExp exp t in
      updateTable id v t'
  (* Might be more nested expressions *)
  | PrintStm exps ->
      let interpretAndPrint t e =
        let v, t' = interpExp e t in
        Stdio.print_endline (Int.to_string v);
        t'
      in
      List.fold_left exps ~init:t ~f:interpretAndPrint

and interpExp e t =
  match e with
  | IdExp i -> (
      match lookupTable (i, t) with
      | Some v -> (v, t)
      | None -> raise (UndefinedVariable i))
  | NumExp i -> (i, t)
  | OpExp (exp1, binop, exp2) ->
      let exp1_val, t' = interpExp exp1 t in
      let exp2_val, _ = interpExp exp2 t' in
      let res =
        match binop with
        | Plus -> exp1_val + exp2_val
        | Minus -> exp1_val - exp2_val
        | Times -> exp1_val * exp2_val
        | Div -> exp1_val / exp2_val
      in
      (res, t')
  | EseqExp (s, e) -> interpExp e (interpStm s t)

let%test_unit "test_max_args" =
  let prog =
    CompoundStm
      ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
        CompoundStm
          ( AssignStm
              ( "b",
                EseqExp
                  ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                    OpExp (NumExp 10, Times, IdExp "a") ) ),
            PrintStm [ IdExp "b" ] ) )
  in
  [%test_eq: int] (maxargs prog) 2

let%expect_test "test-interpretation-simple-program" =
  let prog =
    CompoundStm
      ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
        CompoundStm
          ( AssignStm
              ( "b",
                EseqExp
                  ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                    OpExp (NumExp 10, Times, IdExp "a") ) ),
            PrintStm [ IdExp "b" ] ) )
  in
  ignore (interp prog);
  [%expect {|
    8
    7
    80 |}]
