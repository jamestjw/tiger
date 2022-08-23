open Absyn
open Temp
open Tree
open Frame
open Errormsg
module A = Absyn
module T = Tree

module type TRANSLATE = sig
  type exp
  type level
  type access (* Not the same as Frame.access *)
  type frag

  type new_level_args = {
    parent : level;
    name : Temp.label;
    formals : bool list;
  }

  val outermost : level
  val new_level : new_level_args -> level
  val formals : level -> access list
  val alloc_local : level -> bool -> access
  val default_exp : exp

  (* Access of the variable and the level in which it is used *)
  val simpleVar : access * level -> exp
  val subscriptVar : exp * exp -> exp
  val fieldVar : exp * int -> exp
  val assignExp : exp * exp -> exp
  val arithmeticOperation : exp * A.oper * exp -> exp
  val comparisonOperation : exp * A.oper * exp -> exp
  val ifThenElse : exp * exp * exp -> exp
  val ifThen : exp * exp -> exp
  val stringExp : string -> exp
  val recordExp : exp list -> exp
  val arrayExp : exp * exp -> exp
  val callExp : Temp.label * exp list * level * level -> exp
  val whileExp : exp * exp * Temp.label -> exp
  val forExp : exp * exp * exp * exp * Temp.label -> exp
  val breakExp : Temp.label -> exp
  val seqExp : exp list -> exp
  val procEntryExit : exp * level -> unit
  val getResult : unit -> frag list
  val init : unit -> unit
end

module Translate : TRANSLATE = struct
  type exp =
    | Ex of Tree.exp (* For expressions *)
    | Nx of Tree.stm (* For statements that produce no result *)
    (* Conditional, jumps either to the first or second label depending on
       the statement. *)
    | Cx of (Temp.label * Temp.label -> Tree.stm)

  type level = {
    depth : int;
    frame : Frame.frame;
    unique : unit ref;
    parent : level option;
  }

  type access = level * Frame.access

  type new_level_args = {
    parent : level;
    name : Temp.label;
    formals : bool list;
  }

  type frag = Frame.frag

  let outermost =
    {
      depth = 0;
      frame = Frame.new_frame { name = Temp.named_label "main"; formals = [] };
      unique = ref ();
      parent = None;
    }

  let new_level { name; parent; formals } =
    {
      depth = parent.depth + 1;
      (* Add an extra parameter that escapes to represent the static link *)
      frame = Frame.new_frame { name; formals = true :: formals };
      unique = ref ();
      parent = Some parent;
    }

  let formals l =
    List.map
      (fun f -> (l, f))
      (* Skip the first formal as that is the static link *)
      (List.tl (Frame.formals l.frame))

  let static_link l = List.hd (Frame.formals l.frame)
  let alloc_local l escape = (l, Frame.alloc_local l.frame escape)
  let default_exp = Ex (Tree.CONST 0)

  let rec seq = function
    | [ a ] -> a
    | a :: rest -> T.SEQ (a, seq rest)
    | [] ->
        Errormsg.ErrorMsg.impossible
          "Empty sequences should not be passed to this function"

  let unEx = function
    | Ex e -> e
    | Cx genstm ->
        (* Allocates a register and stores a value of either 0/1
           depending on the conditional statement *)
        let r = Temp.new_temp () in
        let t = Temp.new_label () in
        let f = Temp.new_label () in
        T.ESEQ
          ( seq
              [
                T.MOVE (T.TEMP r, T.CONST 1);
                genstm (t, f);
                T.LABEL f;
                T.MOVE (T.TEMP r, T.CONST 0);
                T.LABEL t;
              ],
            T.TEMP r )
    | Nx s -> T.ESEQ (s, T.CONST 0)

  let unNx = function
    | Ex e -> T.EXP e
    | Cx _ as c -> T.EXP (unEx c)
    | Nx s -> s

  let unCx = function
    | Cx genstm -> genstm
    | Nx _ ->
        Errormsg.ErrorMsg.impossible
          "unCx(Nx _) should never need to be translated"
    | Ex (T.CONST 0) -> fun (_, f) -> T.JUMP (T.NAME f, [ f ])
    | Ex (T.CONST 1) -> fun (t, _) -> T.JUMP (T.NAME t, [ t ])
    | Ex e -> fun (t, f) -> T.CJUMP (T.NE, e, T.CONST 0, t, f)

  let level_equal l1 l2 = l1.unique = l2.unique

  let simpleVar ((original_level, a), l) =
    let rec do_one_level curr_level frame_addr =
      (* If we are at the right level, then just load from this frame *)
      if level_equal original_level curr_level then Frame.exp a frame_addr
      else
        match curr_level.parent with
        | Some parent ->
            (* Otherwise, follow the static link to the parent *)
            do_one_level parent (Frame.exp (static_link curr_level) frame_addr)
        | None -> Errormsg.ErrorMsg.impossible "Missing parent level"
    in
    Ex (do_one_level l (T.TEMP Frame.fp))

  let binOpPlus e1 e2 = Tree.BINOP (Tree.PLUS, e1, e2)
  let binOpMul e1 e2 = Tree.BINOP (Tree.MUL, e1, e2)

  (* TODO: Emit code for bounds checking *)
  let subscriptVar (var_exp, index_exp) =
    Ex
      (T.MEM
         (binOpPlus (unEx var_exp)
            (binOpMul (T.CONST Frame.word_size) (unEx index_exp))))

  (* TODO: Emit code to guard against null pointer deference *)
  let fieldVar (var_exp, field_index) =
    Ex
      (T.MEM
         (binOpPlus (unEx var_exp)
            (binOpMul (T.CONST Frame.word_size) (T.CONST field_index))))

  let assignExp (var, exp) = Nx (T.MOVE (unEx var, unEx exp))

  let arithmeticOperation (left, op, right) =
    let op' =
      match op with
      | A.PlusOp -> T.PLUS
      | A.MinusOp -> T.MINUS
      | A.TimesOp -> T.MUL
      | A.DivideOp -> T.DIV
      | _ -> ErrorMsg.impossible "Invalid arithmetic operator"
    in
    Ex (Tree.BINOP (op', unEx left, unEx right))

  let comparisonOperation (left, op, right) =
    (* TODO: Implement string comparison *)
    let op' =
      match op with
      | A.EqOp -> T.EQ
      | A.NeqOp -> T.NE
      | A.LtOp -> T.LT
      | A.LeOp -> T.LE
      | A.GtOp -> T.GT
      | A.GeOp -> T.GE
      | _ -> ErrorMsg.impossible "Invalid comparison operator"
    in
    Cx (fun (t, f) -> T.CJUMP (op', unEx left, unEx right, t, f))

  let ifThenElse (test, t, f) =
    let t_label = Temp.new_label () in
    let f_label = Temp.new_label () in
    let end_label = Temp.new_label () in
    match t with
    (* If we know that the for statement doesn't have to produce
       a value, we can skip some trouble and produce an Nx directly *)
    | Nx t' ->
        Nx
          (seq
             [
               (unCx test) (t_label, f_label);
               T.LABEL t_label;
               t';
               T.JUMP (T.NAME end_label, [ end_label ]);
               T.LABEL f_label;
               unNx f;
               T.LABEL end_label;
             ])
    | _ ->
        let r = Temp.new_temp () in
        Ex
          (T.ESEQ
             ( seq
                 [
                   (unCx test) (t_label, f_label);
                   T.LABEL t_label;
                   T.MOVE (T.TEMP r, unEx t);
                   T.JUMP (T.NAME end_label, [ end_label ]);
                   T.LABEL f_label;
                   T.MOVE (T.TEMP r, unEx f);
                   T.LABEL end_label;
                 ],
               T.TEMP r ))

  let ifThen (test, t) =
    let t_label = Temp.new_label () in
    let end_label = Temp.new_label () in
    Nx
      (seq
         [
           (unCx test) (t_label, end_label);
           T.LABEL t_label;
           unNx t;
           T.LABEL end_label;
         ])

  let frags : Frame.frag list ref = ref []

  let stringExp s =
    (* Create a new label and attach a string with this
       label to the fragment list *)
    let lab = Temp.new_label () in
    frags := Frame.STRING (lab, s) :: !frags;
    Ex (T.NAME lab)

  let recordExp fields =
    let num_fields = List.length fields in
    let record_size = num_fields * Frame.word_size in
    let r = Temp.new_temp () in
    let _, processed_fields =
      List.fold_left
        (fun (idx, fs) field ->
          ( idx + 1,
            T.MOVE
              ( T.MEM (binOpPlus (T.TEMP r) (T.CONST (idx * Frame.word_size))),
                unEx field )
            :: fs ))
        (0, []) fields
    in
    Ex
      (T.ESEQ
         ( seq
             ([
                T.MOVE
                  ( T.TEMP r,
                    Frame.externalCall ("malloc", [ T.CONST record_size ]) );
              ]
             @ processed_fields),
           T.TEMP r ))

  let arrayExp (size, init) =
    Ex (Frame.externalCall ("initArray", [ unEx size; unEx init ]))

  let findFunctionStaticLink (fn_level, call_level) =
    let rec do_one_level curr_level frame_addr =
      (* If we are at the right level, then just load from this frame *)
      if level_equal fn_level curr_level then
        Frame.exp (static_link fn_level) frame_addr
      else
        match curr_level.parent with
        | Some parent ->
            (* Otherwise, follow the static link to the parent *)
            do_one_level parent (Frame.exp (static_link curr_level) frame_addr)
        | None -> Errormsg.ErrorMsg.impossible "Missing parent level"
    in
    Ex (do_one_level call_level (T.TEMP Frame.fp))

  let callExp (name, args, fn_level, call_level) =
    let call_args = findFunctionStaticLink (fn_level, call_level) :: args in
    Ex (T.CALL (T.NAME name, List.map unEx call_args))

  let whileExp (test, body, end_label) =
    let start_label = Temp.new_label () in
    (* TODO: Do without the body label *)
    let body_label = Temp.new_label () in
    Nx
      (seq
         [
           T.LABEL start_label;
           (unCx test) (body_label, end_label);
           T.LABEL body_label;
           unNx body;
           T.JUMP (T.NAME start_label, [ start_label ]);
           T.LABEL end_label;
         ])

  let forExp (counter, lo, hi, body, end_label) =
    let r = Temp.new_temp () in
    let start_label = Temp.new_label () in
    let increment_label = Temp.new_label () in
    Nx
      (seq
         [
           unNx (assignExp (counter, lo));
           T.MOVE (T.TEMP r, unEx hi);
           (* Check once if lo <= hi *)
           T.CJUMP (T.LE, unEx counter, T.TEMP r, start_label, end_label);
           T.LABEL start_label;
           unNx body;
           T.CJUMP (T.LT, unEx counter, T.TEMP r, increment_label, end_label);
           (* Increment loop counter, we are checking for counter < limit instead
              of counter <= limit as an overflow might occur if limit = maxint, hence
              why we do the check before the increment *)
           T.LABEL increment_label;
           unNx
             (assignExp
                (counter, Ex (Tree.BINOP (Tree.PLUS, unEx counter, T.CONST 1))));
           T.JUMP (T.NAME start_label, [ start_label ]);
           T.LABEL end_label;
         ])

  let breakExp label = Nx (T.JUMP (T.NAME label, [ label ]))

  let seqExp l =
    match l with
    | [ e ] -> e
    | e :: [ r ] -> Ex (T.ESEQ (unNx e, unEx r))
    | _ ->
        let rev = List.rev l in
        let last = List.hd rev in
        let rest = List.rev (List.tl rev) in
        Ex (T.ESEQ (seq (List.map unNx rest), unEx last))

  let procEntryExit (exp, lvl) =
    let body_with_return = T.MOVE (T.TEMP Frame.rv, unEx exp) in
    let processed_body = Frame.procEntryExit1 (lvl.frame, body_with_return) in
    let frag = Frame.PROC { frame = lvl.frame; body = processed_body } in
    frags := frag :: !frags;
    ()

  let getResult () = !frags
  let init () = frags := []
end
