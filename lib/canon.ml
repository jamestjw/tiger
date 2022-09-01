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
  (* val basicBlocks : Tree.stm list -> Tree.stm list list * Temp.label
     val traceSchedule : Tree.stm list list * Temp.label -> Tree.stm list *)
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
end
