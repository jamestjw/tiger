open Assem
open Errormsg
open Frame
open Tree
open Temp
module A = Assem
module T = Tree

module type CODEGEN = sig
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
  val generateString : Assem.label -> string -> Assem.instr
end

module RiscVGen : CODEGEN = struct
  (* Since the calling convention of this ISA uses a real
     frame pointer, we don't need to use a hack to calculate
     the frame pointer, i.e. we ignore the frame argument *)
  let codegen _frame stm =
    let ilist : A.instr list ref = ref [] in
    let emit x = ilist := x :: !ilist in

    let calldefs = [ Frame.rv; Frame.rv ] @ Frame.caller_saves in

    let rec munchStm = function
      | T.SEQ (a, b) ->
          munchStm a;
          munchStm b
      (* Storing to memory address (calculated by adding an expression to a constant) *)
      | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2) ->
          emit
            (A.OPER
               {
                 assem =
                   Printf.sprintf "\taddi 's2, 's0, %d\n\tsd 's1, 0('s2)\n" i;
                 src = [ munchExp e1; munchExp e2; Temp.new_temp () ];
                 dst = [];
                 jump = None;
               })
      (* Same as above, but different order of addition *)
      | T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2) ->
          emit
            (A.OPER
               {
                 assem =
                   Printf.sprintf "\taddi 's2, 's0, %d\n\tsd 's1, 0('s2)\n" i;
                 src = [ munchExp e1; munchExp e2; Temp.new_temp () ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM e1, T.MEM e2) ->
          emit
            (A.OPER
               {
                 (* Dereference s1 and store in s1, then
                    dereference s0 and store s1 there *)
                 assem = "\tld 's1, 0('s1)\n\tsd 's1, 0('s0)\n";
                 src = [ munchExp e1; munchExp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM (T.CONST i), e2) ->
          let t = Temp.new_temp () in
          emit
            (A.OPER
               {
                 (* Store the integer in a register, dereference it and store
                    e2 in it. *)
                 assem = Printf.sprintf "\tli 's0, %d\n\tsd 's1, 0('s0)\n" i;
                 src = [ t; munchExp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM e1, e2) ->
          emit
            (A.OPER
               {
                 (* Dereference e1 and store e2 there *)
                 assem = "\tsd 's1, 0('s0)\n";
                 src = [ munchExp e1; munchExp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.TEMP i, e2) ->
          emit
            (A.MOVE { assem = "\tmv 'd0, 's0\n"; src = munchExp e2; dst = i })
      | T.MOVE (_, _) ->
          ErrorMsg.impossible
            "T.MOVE's first operand has to either be T.TEMP or T.MEM"
      | T.EXP exp -> (
          match exp with
          | T.BINOP (_, e1, e2) ->
              (* Since the result is discarded, we don't bother evaluating
                 the binary operation*)
              ignore (munchExp e1);
              ignore (munchExp e2)
          | T.CALL (e, args) -> (
              match e with
              | T.NAME name ->
                  emit
                    (A.OPER
                       {
                         assem =
                           Printf.sprintf "\tcall %s\n" (A.label_to_string name);
                         (* munchArgs returns list of temporaries that are
                            used in the function call, we put them here so that
                            future liveliness analysis can tell that these values
                            are needed up until the call *)
                         src = munchArgs (0, args);
                         dst = calldefs;
                         jump = None;
                       })
              | _ -> ErrorMsg.impossible "Function must be called with label")
          | T.ESEQ (stm, e) ->
              munchStm stm;
              ignore (munchExp e)
          | _ ->
              (* Since we do not need to yield a value,
                 there is no point evaluating the other
                 expressions *)
              ())
      | T.JUMP (e, _labels) -> (
          match e with
          | T.NAME label ->
              emit
                (A.OPER
                   {
                     (* TODO: Check if unconditional jump works *)
                     assem = "\tj 'j0\n";
                     src = [];
                     dst = [];
                     jump = Some [ label ];
                   })
          | _ -> ErrorMsg.impossible "Impossible to jump to a non-label")
      | T.LABEL lab ->
          emit
            (A.LABEL
               { assem = Printf.sprintf "%s:\n" (A.label_to_string lab); lab })
      | T.CJUMP (op, e1, e2, t_label, _f_label) ->
          let branchOpMap = function
            | T.EQ -> "beq"
            | T.NE -> "bne"
            | T.LT | T.LE -> "blt"
            | T.GE | T.GT -> "bge"
          in
          (* Produces the below code:
              beq a1, a2, true_label
              j , false_label
          *)
          let e1_temp = munchExp e1 in
          let e2_temp = munchExp e2 in
          (* Add 1 to e2 so that we can use LT for this comparison *)
          if op = T.LE then
            emit
              (A.OPER
                 {
                   assem = Printf.sprintf "\taddi 's0, 's0, 1\n";
                   src = [ e2_temp ];
                   dst = [];
                   jump = None;
                 });
          (* Add 1 to e2 so that we can use GE for this comparison *)
          if op = T.GT then
            emit
              (A.OPER
                 {
                   assem = Printf.sprintf "\taddi 's0, 's0, 1\n";
                   src = [ e2_temp ];
                   dst = [];
                   jump = None;
                 });
          (* Since we expect the false label to follow a CJUMP, we can just fall-through to it *)
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\t%s 's0, 's1, 'j0\n" (branchOpMap op);
                 src = [ e1_temp; e2_temp ];
                 dst = [];
                 jump = Some [ t_label ];
               })
    and munchExp exp =
      let result gen =
        let t = Temp.new_temp () in
        gen t;
        t
      in
      match exp with
      | T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     (* Dereference and store in a register *)
                     assem =
                       Printf.sprintf "\taddi 'd0, 's0, %d\n\tld 'd0, 0('d0)\n"
                         i;
                     src = [ munchExp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      (* Same as above *)
      | T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem =
                       Printf.sprintf "\taddi 'd0, 's0, %d\n\tld 'd0, 0('d0)\n"
                         i;
                     src = [ munchExp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.MEM (T.CONST i) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     (* Put the integer in a register and dereference it *)
                     assem = Printf.sprintf "\tli 'd0, %d\n\tld 'd0, 0('d0)\n" i;
                     src = [];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.MEM e1 ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = "\tld 'd0, 0('s0)\n";
                     src = [ munchExp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.BINOP (T.PLUS, e1, T.CONST i) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\taddi 'd0, 's0, %d\n" i;
                     src = [ munchExp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.BINOP (T.PLUS, T.CONST i, e1) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\taddi 'd0, 's0, %d\n" i;
                     src = [ munchExp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.BINOP (op, e1, e2) ->
          let opMap = function
            | T.PLUS -> "add"
            | T.MINUS -> "sub"
            | T.MUL -> "mul"
            | T.DIV -> "div"
            | T.AND -> "and"
            | T.OR -> "or"
            | T.XOR -> "xor"
          in
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\t%s 'd0, 's0, 's1\n" (opMap op);
                     src = [ munchExp e1; munchExp e2 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.CONST i ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\tli 'd0, %d\n" i;
                     src = [];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.TEMP t -> t
      | T.CALL (e, args) -> (
          match e with
          | T.NAME name ->
              result (fun r ->
                  emit
                    (A.OPER
                       {
                         (* Call function and store return value in a register to return *)
                         assem =
                           Printf.sprintf "\tcall %s\n\tmv 'd0, 'd1\n"
                             (A.label_to_string name);
                         (* munchArgs returns list of temporaries that are
                            used in the function call, we put them here so that
                            future liveliness analysis can tell that these values
                            are needed up until the call *)
                         src = munchArgs (0, args);
                         dst = r :: Frame.rv :: calldefs;
                         jump = None;
                       }))
          | _ -> ErrorMsg.impossible "Function must be called with label")
      | T.NAME lab ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem =
                       Printf.sprintf
                         "\tlui 'd0, %%hi(%s)\n\taddi 'd0, 'd0, %%lo(%s)\n"
                         (A.label_to_string lab) (A.label_to_string lab);
                     src = [];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.ESEQ (stm, e) ->
          munchStm stm;
          munchExp e
    (* Returns temps corresponding to registers used *)
    and munchArgs (i, args) =
      match args with
      | arg :: rest -> (
          (* Either write argument to register or to memory *)
          match List.nth_opt Frame.arg_regs i with
          | Some reg ->
              emit
                (A.MOVE
                   { assem = "\tmv 'd0, 's0\n"; src = munchExp arg; dst = reg });
              (* Include register used in the return list *)
              reg :: munchArgs (i + 1, rest)
          | None ->
              let offset = Frame.word_size * (i - List.length Frame.arg_regs) in
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\tsd 's0, %d('d0)\n" offset;
                     src = [ munchExp arg ];
                     dst = [ Frame.fp ];
                     jump = None;
                   });

              munchArgs (i + 1, rest))
      | [] -> []
    in

    munchStm stm;
    List.rev !ilist

  let generateString lab str =
    A.LABEL
      {
        assem =
          Printf.sprintf "\t.text\n\t.section\t.rodata\n%s:\n\t.string \"%s\"\n"
            (A.label_to_string lab) str;
        lab;
      }

  (* Tests *)

  open Base
  open Canon
  open Semant
  open Translate

  let canonize stm =
    Canon.linearize stm |> Canon.basicBlocks |> Canon.traceSchedule

  let%test_unit "test_simple_function_call_with_addition" =
    Temp.reset ();

    let frame =
      Frame.new_frame { name = Temp.named_label "main"; formals = [] }
    in
    let input_string =
      {|
      let 
        var x := 8
        var y := 10
      in 
        print(chr(x + y))
      end
    |}
    in
    let exp, _ = Semant.transProg (Parser.parse_string input_string) in
    let stm = canonize (Translate.unNx exp) in
    let res =
      Stdlib.List.flatten (List.map ~f:(fun s -> codegen frame s) stm)
    in
    let expected =
      [
        "L51:\n";
        "\tli t135, 10\n";
        "\taddi t134, fp, -32\n\tsd t135, 0(t134)\n";
        "\tli t137, 8\n";
        "\taddi t136, fp, -24\n\tsd t137, 0(t136)\n";
        "\taddi t138, fp, 0\n\tld t138, 0(t138)\n";
        "\tmv t133, t138\n";
        "\taddi t140, fp, 0\n\tld t140, 0(t140)\n";
        "\tmv a1, t140\n";
        "\taddi t142, fp, -32\n\tld t142, 0(t142)\n";
        "\taddi t143, fp, -24\n\tld t143, 0(t143)\n";
        "\tadd t141, t143, t142\n";
        "\tmv a2, t141\n";
        "\tcall chr\n\tmv t139, a0\n";
        "\tmv t132, t139\n";
        "\tmv a1, t133\n";
        "\tmv a2, t132\n";
        "\tcall print\n";
        "\tj L50\n";
        "L50:\n";
      ]
    in
    [%test_eq: string list]
      (List.map ~f:(Assem.format Frame.registerToString) res)
      expected

  let%test_unit "test_if_statement" =
    Temp.reset ();

    let frame =
      Frame.new_frame { name = Temp.named_label "main"; formals = [] }
    in
    let input_string =
      {|
      let 
        var x := 8
        var y := 10
      in 
        if x < y then 20 else 30
      end
    |}
    in
    let exp, _ = Semant.transProg (Parser.parse_string input_string) in
    let stm = canonize (Translate.unNx exp) in
    let res =
      Stdlib.List.flatten (List.map ~f:(fun s -> codegen frame s) stm)
    in
    let expected =
      [
        "L54:\n";
        "\tli t132, 10\n";
        "\taddi t131, fp, -32\n\tsd t132, 0(t131)\n";
        "\tli t134, 8\n";
        "\taddi t133, fp, -24\n\tsd t134, 0(t133)\n";
        "\taddi t135, fp, -24\n\tld t135, 0(t135)\n";
        "\taddi t136, fp, -32\n\tld t136, 0(t136)\n";
        "\tblt t135, t136, L50\n";
        "L51:\n";
        "\tli t137, 30\n";
        "\tmv t130, t137\n";
        "L52:\n";
        "\tj L53\n";
        "L50:\n";
        "\tli t138, 20\n";
        "\tmv t130, t138\n";
        "\tj L52\n";
        "L53:\n";
      ]
    in
    [%test_eq: string list]
      (List.map ~f:(Assem.format Frame.registerToString) res)
      expected

  let%test_unit "test_for_loop" =
    Temp.reset ();

    let frame =
      Frame.new_frame { name = Temp.named_label "main"; formals = [] }
    in
    let input_string = {|
      for i := 0 to 10 do exit(i)
    |} in
    let exp, _ = Semant.transProg (Parser.parse_string input_string) in
    let stm = canonize (Translate.unNx exp) in
    let res =
      Stdlib.List.flatten (List.map ~f:(fun s -> codegen frame s) stm)
    in
    let expected =
      [
        "L54:\n";
        "\tli t133, 0\n";
        "\taddi t132, fp, -24\n\tsd t133, 0(t132)\n";
        "\tli t134, 10\n";
        "\tmv t131, t134\n";
        "\taddi t135, fp, -24\n\tld t135, 0(t135)\n";
        "\taddi t131, t131, 1\n";
        "\tblt t135, t131, L51\n";
        "L50:\n";
        "\tj L53\n";
        "L51:\n";
        "\taddi t136, fp, 0\n\tld t136, 0(t136)\n";
        "\tmv a1, t136\n";
        "\taddi t137, fp, -24\n\tld t137, 0(t137)\n";
        "\tmv a2, t137\n";
        "\tcall exit\n";
        "\taddi t138, fp, -24\n\tld t138, 0(t138)\n";
        "\tbge t138, t131, L50\n";
        "L52:\n";
        "\taddi t141, fp, -24\n\tld t141, 0(t141)\n";
        "\taddi t140, t141, 1\n";
        "\taddi t139, fp, -24\n\tsd t140, 0(t139)\n";
        "\tj L51\n";
        "L53:\n";
      ]
    in
    [%test_eq: string list]
      (List.map ~f:(Assem.format Frame.registerToString) res)
      expected

  let%test_unit "test_while_loop" =
    Temp.reset ();

    let frame =
      Frame.new_frame { name = Temp.named_label "main"; formals = [] }
    in
    let input_string =
      {|
      let
        var i := 0
        var max := 10
      in
        while i < max
        do (print(chr(i)); i := i + 1)
      end
    |}
    in
    let exp, _ = Semant.transProg (Parser.parse_string input_string) in
    let stm = canonize (Translate.unNx exp) in
    let res =
      Stdlib.List.flatten (List.map ~f:(fun s -> codegen frame s) stm)
    in
    let expected =
      [
        "L54:\n";
        "\tli t135, 10\n";
        "\taddi t134, fp, -32\n\tsd t135, 0(t134)\n";
        "\tli t137, 0\n";
        "\taddi t136, fp, -24\n\tsd t137, 0(t136)\n";
        "L51:\n";
        "\taddi t138, fp, -24\n\tld t138, 0(t138)\n";
        "\taddi t139, fp, -32\n\tld t139, 0(t139)\n";
        "\tblt t138, t139, L52\n";
        "L50:\n";
        "\tj L53\n";
        "L52:\n";
        "\taddi t140, fp, 0\n\tld t140, 0(t140)\n";
        "\tmv t133, t140\n";
        "\taddi t142, fp, 0\n\tld t142, 0(t142)\n";
        "\tmv a1, t142\n";
        "\taddi t143, fp, -24\n\tld t143, 0(t143)\n";
        "\tmv a2, t143\n";
        "\tcall chr\n\tmv t141, a0\n";
        "\tmv t132, t141\n";
        "\tmv a1, t133\n";
        "\tmv a2, t132\n";
        "\tcall print\n";
        "\taddi t146, fp, -24\n\tld t146, 0(t146)\n";
        "\taddi t145, t146, 1\n";
        "\taddi t144, fp, -24\n\tsd t145, 0(t144)\n";
        "\tj L51\n";
        "L53:\n";
      ]
    in
    [%test_eq: string list]
      (List.map ~f:(Assem.format Frame.registerToString) res)
      expected
end
