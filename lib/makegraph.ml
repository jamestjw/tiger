open Assem
open Flowgraph
open Symbol
open Errormsg

module MakeGraph = struct
  (* The function instrs2graph takes a list of instructions
     and returns a flow graph, along with a list of nodes that
     corresponds exactly to the instructions *)
  let instrs2graph instrs =
    let graph = Flow.Graph.newGraph () in
    let handle_instr instr (def_tbl, use_tbl, move_tbl, label2node) =
      let newNode = Flow.Graph.newNode graph in
      ( (match instr with
        | Assem.OPER { assem; dst; src; jump } ->
            (match jump with
            | Some symbols ->
                List.iter
                  (fun symbol ->
                    match Symbol.look (label2node, symbol) with
                    | Some node ->
                        Flow.Graph.mk_edge { src = newNode; dst = node }
                    | None -> ErrorMsg.impossible "Jumping to an unknown label")
                  symbols
            | None -> ());
            ( Flow.Graph.Table.enter (def_tbl, newNode, dst),
              Flow.Graph.Table.enter (use_tbl, newNode, src),
              Flow.Graph.Table.enter (move_tbl, newNode, false),
              label2node )
        | Assem.LABEL { assem; lab } ->
            ( Flow.Graph.Table.enter (def_tbl, newNode, []),
              Flow.Graph.Table.enter (use_tbl, newNode, []),
              Flow.Graph.Table.enter (move_tbl, newNode, false),
              Symbol.enter (label2node, lab, newNode) )
        | Assem.MOVE { assem; dst; src } ->
            ( Flow.Graph.Table.enter (def_tbl, newNode, [ dst ]),
              Flow.Graph.Table.enter (use_tbl, newNode, [ src ]),
              Flow.Graph.Table.enter (move_tbl, newNode, true),
              label2node )),
        newNode )
    in

    let nodes, def_tbl, use_tbl, move_tbl, _ =
      List.fold_left
        (fun (l, dtbl, utbl, mtbl, label2node) instr ->
          let (dtbl', utbl', mtbl', label2node'), l' =
            handle_instr instr (dtbl, utbl, mtbl, label2node)
          in
          (l' :: l, dtbl', utbl', mtbl', label2node'))
        ( [],
          Flow.Graph.Table.empty,
          Flow.Graph.Table.empty,
          Flow.Graph.Table.empty,
          Symbol.empty )
        (* We want to process from the back *)
        (List.rev instrs)
    in
    let fg =
      Flow.FGRAPH
        { control = graph; def = def_tbl; use = use_tbl; ismove = move_tbl }
    in
    (fg, nodes)
end

open Base
open Temp

let%test_unit "make_graph_simple_block" =
  (* Based on tests/test1.tig *)
  let l0 = Temp.new_label () in
  let t130 = Temp.new_temp () in
  let t131 = Temp.new_temp () in
  let t132 = Temp.new_temp () in
  let t133 = Temp.new_temp () in
  let a1 = Temp.new_temp () in
  let a2 = Temp.new_temp () in
  let rv = Temp.new_temp () in
  let instrs =
    (* Tuple of (instr, move, def, use) *)
    [
      (Assem.LABEL { assem = "L1:\n"; lab = Temp.new_label () }, false);
      ( Assem.OPER
          { assem = "li t132, 10\n"; dst = [ t132 ]; src = []; jump = None },
        false );
      (Assem.MOVE { assem = "mv a1, t132\n"; dst = a1; src = t132 }, true);
      ( Assem.OPER
          { assem = "li t133, 0\n"; dst = [ t133 ]; src = []; jump = None },
        false );
      (Assem.MOVE { assem = "mv a2, t133\n"; dst = a2; src = t133 }, true);
      ( Assem.OPER
          { assem = "call initArray\n"; dst = [ rv ]; src = []; jump = None },
        false );
      (Assem.MOVE { assem = "mv t131, a0\n"; dst = t131; src = rv }, true);
      (Assem.MOVE { assem = "mv t130, t131\n"; dst = t130; src = t131 }, true);
      ( Assem.OPER { assem = "j L0\n"; dst = []; src = []; jump = Some [ l0 ] },
        false );
      (Assem.LABEL { assem = "L0:\n"; lab = l0 }, false);
    ]
  in
  let flowgraph, nodes =
    MakeGraph.instrs2graph (List.map ~f:(fun (instr, _) -> instr) instrs)
  in
  List.iter2_exn nodes instrs ~f:(fun node (instr, ismove) ->
      [%test_eq: bool]
        (let (Flow.FGRAPH { ismove = ismove_tbl; _ }) = flowgraph in
         Option.value_exn (Flow.Graph.Table.look (ismove_tbl, node)))
        ismove)
