(* The purpose of this module is to convert a Tree
   to its canonical form, i.e. the simplest or
   clearest schema possible *)

open Temp
open Tree
open Errormsg
module T = Tree

module type CANON = sig
  (* From an arbitrary Tree statement, produce a list of cleaned trees
     satisfying the following properties:
      1.  No SEQ's or ESEQ's
      2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
  *)
  val linearize : Tree.stm -> Tree.stm list

  (* From a list of cleaned trees, produce a list of
      basic blocks satisfying the following properties:
        1. and 2. as above;
        3.  Every block begins with a LABEL;
        4.  A LABEL appears only at the beginning of a block;
        5.  Any JUMP or CJUMP is the last stm in a block;
        6.  Every block ends with a JUMP or CJUMP;
     Also produce the "label" to which control will be passed
     upon exit. *)
  val basicBlocks : Tree.stm list -> Tree.stm list list * Temp.label
  (* val traceSchedule : Tree.stm list list * Temp.label -> Tree.stm list *)
end

module Canon : CANON = struct
  (* Estimates (naively) if a statement and expression commute, i.e.
     switching the order of execution doesn't affect the result *)

  let linearize stm0 =
    let ( % ) x y =
      match (x, y) with
      | T.EXP (T.CONST _), _ -> y
      | _, T.EXP (T.CONST _) -> x
      | _, _ -> T.SEQ (x, y)
    in
    let commute = function
      | T.EXP (T.CONST _), _ -> true
      | _, T.NAME _ -> true
      | _, T.CONST _ -> true
      | _ -> false
    in
    let nop = T.EXP (T.CONST 0) in

    let rec reorder = function
      | (T.CALL _ as e) :: rest ->
          let t = Temp.new_temp () in
          reorder (T.ESEQ (T.MOVE (T.TEMP t, e), T.TEMP t) :: rest)
      | a :: rest ->
          let stms, e = do_exp a in
          let stms', el = reorder rest in
          if commute (stms', e) then (stms % stms', e :: el)
          else
            let t = Temp.new_temp () in
            (stms % T.MOVE (T.TEMP t, e) % stms', T.TEMP t :: el)
      | [] -> (nop, [])
    (* Similar to reorder_stm, but it returns a pair (s, e) where s is a
       statement containing all the side effects pulled out of the list of
       expressions, and e is build(l') *)
    and reorder_exp (el, build) =
      let stms, el' = reorder el in
      (stms, build el')
    (* Pulls all the ESEQs out of the list, yielding a statement s' that
        contains all the statements from the ESEQs and a list l' of
        cleaned-up expressions. Then makes SEQ(s',build(l')) *)
    and reorder_stm (el, build) =
      let stms, el' = reorder el in
      stms % build el'
    and do_stm = function
      | T.SEQ (a, b) -> do_stm a % do_stm b
      | T.JUMP (e, labs) ->
          reorder_stm
            ( [ e ],
              function
              | [ e ] -> T.JUMP (e, labs)
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | T.CJUMP (p, a, b, t, f) ->
          reorder_stm
            ( [ a; b ],
              function
              | [ a; b ] -> T.CJUMP (p, a, b, t, f)
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | T.MOVE (T.TEMP t, T.CALL (e, el)) ->
          reorder_stm
            ( e :: el,
              function
              | e :: el -> T.MOVE (T.TEMP t, T.CALL (e, el))
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | T.MOVE (T.TEMP t, b) ->
          reorder_stm
            ( [ b ],
              function
              | [ b ] -> T.MOVE (T.TEMP t, b)
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | T.MOVE (T.MEM e, b) ->
          reorder_stm
            ( [ e; b ],
              function
              | [ e; b ] -> T.MOVE (T.MEM e, b)
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | T.MOVE (T.ESEQ (s, e), b) -> do_stm (T.SEQ (s, T.MOVE (e, b)))
      | T.EXP (T.CALL (e, el)) ->
          reorder_stm
            ( e :: el,
              function
              | e :: el -> T.EXP (T.CALL (e, el))
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | T.EXP e ->
          reorder_stm
            ( [ e ],
              function
              | [ e ] -> T.EXP e
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | s ->
          reorder_stm
            ( [],
              function
              | [] -> s
              | _ -> ErrorMsg.impossible "This should never be possible" )
    and do_exp = function
      | T.BINOP (p, a, b) ->
          reorder_exp
            ( [ a; b ],
              function
              | [ a; b ] -> T.BINOP (p, a, b)
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | T.MEM a ->
          reorder_exp
            ( [ a ],
              function
              | [ a ] -> T.MEM a
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | T.ESEQ (s, e) ->
          let stms = do_stm s in
          let stms', e = do_exp e in
          (stms % stms', e)
      | T.CALL (e, el) ->
          reorder_exp
            ( e :: el,
              function
              | e :: el -> T.CALL (e, el)
              | _ -> ErrorMsg.impossible "This should never be possible" )
      | e ->
          reorder_exp
            ( [],
              function
              | [] -> e
              | _ -> ErrorMsg.impossible "This should never be possible" )
    in
    (* linear gets rid of the top-level SEQ's, producing a list *)
    let rec linear = function
      | T.SEQ (a, b), l -> linear (a, linear (b, l))
      | s, l -> s :: l
    in
    linear (do_stm stm0, [])

  let basicBlocks stms =
    let done_label = Temp.new_label () in
    (* Pass in remaining statements and an accumulator list *)
    let rec blocks = function
      | (T.LABEL _ as head) :: tail, blist ->
          let rec next = function
            (* A block is ended when a JUMP/CJUMP is encountered *)
            | (T.JUMP _ as s) :: rest, thisblock ->
                endblock (rest, s :: thisblock)
            | (T.CJUMP _ as s) :: rest, thisblock ->
                endblock (rest, s :: thisblock)
            (* A new block is started when a LABEL is encountered *)
            | (T.LABEL lab :: _ as stms), thisblock ->
                next (T.JUMP (T.NAME lab, [ lab ]) :: stms, thisblock)
            | s :: rest, thisblock -> next (rest, s :: thisblock)
            (* When we run out of statements, end the block with a
               jump to the done label *)
            | [], thisblock ->
                next ([ T.JUMP (T.NAME done_label, [ done_label ]) ], thisblock)
          and endblock (stms, thisblock) =
            blocks (stms, List.rev thisblock :: blist)
          in
          next (tail, [ head ])
      | [], blist -> List.rev blist
      (* If a block doesn't start with a label, we stick an invented
         label at the beginning *)
      | stms, blist -> blocks (T.LABEL (Temp.new_label ()) :: stms, blist)
    in
    (blocks (stms, []), done_label)
end
