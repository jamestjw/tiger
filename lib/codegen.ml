open Base
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

  (* For now, the below 2 functions are used to load and store spills *)
  val generate_store : Temp.temp * Frame.access -> Assem.instr
  val generate_fetch : Temp.temp * Frame.access -> Assem.instr
end

module RiscVGen : CODEGEN = struct
  (* Since the calling convention of this ISA uses a real
     frame pointer, we don't need to use a hack to calculate
     the frame pointer, i.e. we ignore the frame argument *)
  let codegen frame stm =
    let ilist : A.instr list ref = ref [] in
    let emit x = ilist := x :: !ilist in
    (* The callee has no obligation to preserve these registers. *)
    let calldefs =
      [ Frame.ra; Frame.rv ] @ Frame.caller_saves @ Frame.arg_regs
      |> List.dedup_and_sort ~compare:Temp.compare_temp
    in

    let rec munchStm = function
      | T.SEQ (a, b) ->
          munchStm a;
          munchStm b
      (* Storing to memory address (calculated by adding an expression to a constant) *)
      | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2)
      | T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2) ->
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\tsd 's1, %d('s0)\n" i;
                 src = [ munchExp e1; munchExp e2 ];
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
      | T.JUMP (e, labels) -> (
          match e with
          | T.NAME label ->
              emit
                (A.OPER
                   {
                     (* TODO: Check if unconditional jump works *)
                     assem = "\tj 'j0\n";
                     src = [];
                     dst = [];
                     jump = Some labels;
                   })
          | _ ->
              (* TODO: `e` could be some kind of address that is calculated, we
                 should support this. *)
              ErrorMsg.impossible "Impossible to jump to a non-label")
      | T.LABEL lab ->
          emit
            (A.LABEL
               { assem = Printf.sprintf "%s:\n" (A.label_to_string lab); lab })
      | T.CJUMP (op, e1, e2, t_label, f_label) ->
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
          let e2_temp =
            match op with
            | T.LE | T.GT ->
                (* Add 1 to e2 so that we can use LT instead of LE or GE instead
                   of GT for this comparison *)
                let temp = Temp.new_temp () in
                emit
                  (A.OPER
                     {
                       assem = Printf.sprintf "\taddi 'd0, 's0, 1\n";
                       src = [ e2_temp ];
                       dst = [ temp ];
                       jump = None;
                     });
                temp
            | _ -> e2_temp
          in
          (* Although we fall through to the false label, we include the jump
             destination here for future liveness analysis. *)
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\t%s 's0, 's1, 'j0\n" (branchOpMap op);
                 src = [ e1_temp; e2_temp ];
                 dst = [];
                 jump = Some [ t_label; f_label ];
               })
    and munchExp exp =
      let result gen =
        let t = Temp.new_temp () in
        gen t;
        t
      in
      match exp with
      | T.MEM (T.BINOP (T.PLUS, e1, T.CONST i))
      | T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     (* Dereference and store in a register *)
                     assem = Printf.sprintf "\tld 'd0, %d('s0)\n" i;
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
      | T.BINOP (T.PLUS, e1, T.CONST i) | T.BINOP (T.PLUS, T.CONST i, e1) ->
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
      | T.CONST 0 ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = "\tmv 'd0, 's0\n";
                     src = [ Frame.zero ];
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
                           Printf.sprintf "\tcall %s\n" (A.label_to_string name);
                         (* munchArgs returns list of temporaries that are
                            used in the function call, we put them here so that
                            future liveliness analysis can tell that these values
                            are needed up until the call *)
                         src = munchArgs (0, args);
                         dst = calldefs;
                         jump = None;
                       });
                  emit
                    (A.MOVE
                       { assem = "\tmv 'd0, 's0\n"; src = Frame.rv; dst = r }))
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
          match List.nth Frame.arg_regs i with
          | Some reg ->
              emit
                (A.MOVE
                   { assem = "\tmv 'd0, 's0\n"; src = munchExp arg; dst = reg });
              (* Include register used in the return list *)
              reg :: munchArgs (i + 1, rest)
          | None ->
              (* TODO: Verify that this is right *)
              (* Want to put the arguments in reverse order on the stack *)
              let offset =
                Frame.word_size
                * (i + 1 - List.length Frame.arg_regs - Frame.num_locals frame)
              in
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\tsd 's0, %d('s1)\n" offset;
                     src = [ munchExp arg; Frame.fp ];
                     dst = [];
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
          Printf.sprintf "\t.section\t.rodata\n%s:\n\t.string \"%s\"\n"
            (A.label_to_string lab) str;
        lab;
      }

  let generate_store (temp, access) =
    match access with
    | Frame.InFrame offset ->
        A.OPER
          {
            assem = Printf.sprintf "\tsd 's0, %d('s1)\n" offset;
            src = [ temp; Frame.fp ];
            dst = [];
            jump = None;
          }
    | _ -> failwith "Not implemented yet."

  let generate_fetch (temp, access) =
    match access with
    | Frame.InFrame offset ->
        A.OPER
          {
            assem = Printf.sprintf "\tld 'd0, %d('s0)\n" offset;
            src = [ Frame.fp ];
            dst = [ temp ];
            jump = None;
          }
    | _ -> failwith "Not implemented yet."

  (* Tests *)

  open Base
  open Canon
  open Semant

  let generateFunctionStm stm frame =
    let stm = Canon.canonize stm in
    let instrs =
      Frame.procEntryExit2
        (frame, Stdlib.List.flatten (List.map ~f:(codegen frame) stm))
    in
    Frame.procEntryExit3 (frame, instrs)
    |> Frame.fn_prolog_epilog_to_string ~register_map:(Frame.get_temp_map ())

  let generateFrag = function
    | Frame.PROC { body; frame } -> generateFunctionStm body frame
    | Frame.STRING (lab, str) ->
        Assem.format Frame.register_to_string_default (generateString lab str)

  let%test_unit "test_codegen_test_files" =
    let test_dir = "../../../tests/codegen/" in
    Stdlib.Sys.readdir test_dir
    |> Array.to_list
    |> List.filter ~f:(fun x -> String.(Stdlib.Filename.extension x = ".tig"))
    |> List.iter ~f:(fun fname ->
           let expected_fname =
             test_dir ^ Stdlib.Filename.chop_extension fname ^ ".s"
           in
           let expected =
             In_channel.with_open_bin expected_fname In_channel.input_all
           in
           let frags =
             Semant.transProg (Parser.parse_file (test_dir ^ fname))
           in
           let res =
             List.map ~f:generateFrag frags |> List.fold ~init:"" ~f:( ^ )
           in
           [%test_result: string] res ~expect:expected)
end
