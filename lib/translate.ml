open Absyn
open Temp
open Tree
open Frame
open Errormsg
open Types
module A = Absyn
module T = Tree

(* module type TRANSLATE = sig
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
     val alloc_local : level -> bool -> int -> access
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
     val intExp : int -> exp
     val procEntryExit : exp * level -> unit
     val getResult : unit -> frag list
     val init : unit -> unit
     val unNx : exp -> T.stm
   end *)

module Translate = struct
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
    (* To know if calling this function requires adding a static link *)
    static : bool;
  }
  [@@deriving show]

  type access = level * Frame.access [@@deriving show]
  type frag = Frame.frag

  module StringMap = Stdlib.Map.Make (String)

  let outermost =
    {
      depth = 0;
      frame =
        Frame.new_frame { name = Temp.named_label "outermost"; formals = [] };
      unique = ref ();
      parent = None;
      static = true;
    }

  let new_level ~name ~parent ~formals ~static =
    {
      depth = parent.depth + 1;
      (* Add an extra parameter that escapes to represent the static link *)
      frame = Frame.new_frame { name; formals = true :: formals };
      unique = ref ();
      parent = Some parent;
      static;
    }

  let formals l =
    List.map
      (fun f -> (l, f))
      (* Skip the first formal as that is the static link *)
      (List.tl (Frame.formals l.frame))

  let static_link l = List.hd (Frame.formals l.frame)
  let alloc_local l escape = (l, Frame.alloc_local l.frame escape)
  let default_exp = Ex (Tree.CONST 0)
  let frags : Frame.frag list ref = ref []
  let str_labels : Temp.label StringMap.t ref = ref StringMap.empty

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

  let level_equal l1 l2 = l1.unique == l2.unique

  let simpleVar ((var_level, a), l) =
    let rec do_one_level curr_level frame_addr =
      (* If we are at the right level, then just load from this frame *)
      if level_equal var_level curr_level then Frame.exp a frame_addr
      else
        match curr_level.parent with
        | Some parent ->
            (* Otherwise, follow the static link to the parent *)
            do_one_level parent (Frame.exp (static_link curr_level) frame_addr)
        | None -> Errormsg.ErrorMsg.impossible "Missing parent level"
    in
    Ex (do_one_level l (T.TEMP Frame.fp))

  let stringExp s =
    (* Create a new label and attach a string with this
       label to the fragment list *)
    let lab =
      match StringMap.find_opt s !str_labels with
      | Some l -> l
      | None ->
          let lab = Temp.new_label () in
          frags := Frame.STRING (lab, s) :: !frags;
          str_labels := StringMap.add s lab !str_labels;
          lab
    in
    Ex (T.NAME lab)

  let binOpPlus e1 e2 = Tree.BINOP (Tree.PLUS, e1, e2)
  let binOpMul e1 e2 = Tree.BINOP (Tree.MUL, e1, e2)

  (* TODO: Emit code for bounds checking.
     Idea: Use the first word to store the array length
     and use that to carry out bounds checking. *)
  let subscriptVar (var_exp, index_exp) =
    Ex
      (T.MEM
         (binOpPlus (unEx var_exp)
            (binOpMul (T.CONST Frame.word_size) (unEx index_exp))))

  (* Assume that stdlib functions do not require a static link *)
  let callStdlibExp (name, args) =
    Ex (Frame.externalCall (name, List.map unEx args))

  let fieldVar (var_exp, field_index) =
    (* Store the record pointer in a register so we can
       check that it is not null *)
    let r = Temp.new_temp () in
    (* NOTE: This was previously used  when checking for null pointers. *)
    (* let quit_label = Temp.new_label () in *)
    (* let ok_label = Temp.new_label () in *)
    Ex
      (T.ESEQ
         ( seq
             [
               T.MOVE (T.TEMP r, unEx var_exp);
               unNx (callStdlibExp ("assert_non_null", [ Ex (T.TEMP r) ]));
               (* TODO: Generating the following code everytime there is a field
                  access adds bloat to the output code and also slows down register
                  allocation. One day when we have a runtime library written in
                  Tiger, we should move this there, but for now, we shall call the runtime fn
                  written in C to assert that the pointer is non null. *)

               (* T.CJUMP (T.EQ, T.TEMP r, T.CONST 0, quit_label, ok_label); *)
               (* T.LABEL quit_label; *)
               (* (* If we encounter a null pointer, print a message and exit *) *)
               (* unNx *)
               (* (callStdlibExp *)
               (* ( Temp.named_label "print", *)
               (* [ stringExp "Null pointer dereference" ] )); *)
               (* unNx *)
               (* (callStdlibExp (Temp.named_label "exit", [ Ex (T.CONST 1) ])); *)
               (* T.LABEL ok_label; *)
             ],
           (* TODO: We manually did the constant folding here, though it
              wouldn't been necessary if the compiler implemented constant
              folding as an optimisation. *)
           T.MEM
             (binOpPlus (T.TEMP r) (T.CONST (Frame.word_size * field_index))) ))

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

  let comparisonOperation (left, op, right, left_type, right_type) =
    let op =
      match op with
      | A.EqOp -> T.EQ
      | A.NeqOp -> T.NE
      | A.LtOp -> T.LT
      | A.LeOp -> T.LE
      | A.GtOp -> T.GT
      | A.GeOp -> T.GE
      | _ -> ErrorMsg.impossible "Invalid comparison operator"
    in
    match (left_type, right_type, op) with
    | Types.INT, Types.INT, op ->
        Some (Cx (fun (t, f) -> T.CJUMP (op, unEx left, unEx right, t, f)))
    | (Types.NIL | Types.RECORD _), (Types.RECORD _ | Types.NIL), (T.EQ | T.NE)
      ->
        Some (Cx (fun (t, f) -> T.CJUMP (op, unEx left, unEx right, t, f)))
    | Types.STRING, Types.STRING, op ->
        let cmp_res = callStdlibExp ("strcmp", [ left; right ]) in
        Some (Cx (fun (t, f) -> T.CJUMP (op, unEx cmp_res, T.CONST 0, t, f)))
    | _ -> None

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
                    unEx
                    @@ callStdlibExp ("malloc", [ Ex (T.CONST record_size) ]) );
              ]
             @ processed_fields),
           T.TEMP r ))

  let arrayExp (size, init) = callStdlibExp ("initArray", [ size; init ])

  let findFunctionStaticLink (fn_level, call_level) =
    let rec do_one_level curr_level frame_addr =
      (* If we are at the right level, then just load from this frame *)
      if level_equal fn_level curr_level then frame_addr
      else
        match curr_level.parent with
        | Some parent ->
            (* Otherwise, follow the static link to the parent *)
            do_one_level parent (Frame.exp (static_link curr_level) frame_addr)
        | None -> Errormsg.ErrorMsg.impossible "Missing parent level"
    in
    Ex (do_one_level call_level (T.TEMP Frame.fp))

  let callExp (name, args, { static; parent; _ }, call_level) =
    let call_args =
      match (static, parent) with
      | false, _ -> args
      | true, Some parent_level ->
          findFunctionStaticLink (parent_level, call_level) :: args
      | true, None ->
          Errormsg.ErrorMsg.impossible "Called function without parent"
    in
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
    | [] -> Ex (T.CONST 0)
    | [ e ] -> e
    | l ->
        let rev = List.rev l in
        let last = List.hd rev in
        let rest = List.rev (List.tl rev) in
        Ex (T.ESEQ (seq (List.map unNx rest), unEx last))

  let intExp i = Ex (T.CONST i)

  let procEntryExit (exp, { frame; _ }) =
    let body_with_return = T.MOVE (T.TEMP Frame.rv, unEx exp) in
    let processed_body = Frame.procEntryExit1 (frame, body_with_return) in
    let frag = Frame.PROC { frame; body = processed_body } in
    frags := frag :: !frags;
    ()

  let getResult () = !frags

  let init () =
    frags := [];
    str_labels := StringMap.empty
end
