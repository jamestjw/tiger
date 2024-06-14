open Assem
open Flowgraph
open Symbol
open Errormsg

module MakeGraph = struct
  (* The function instrs2graph takes a list of instructions
     and returns a flow graph, along with a list of nodes that
     corresponds exactly to the instructions *)
  let instrs2graph instrs : Flow.flowgraph * Flow.Graph.node list =
    let graph = Flow.Graph.newGraph () in
    let handle_instr instr
        (prev_node, def_tbl, use_tbl, move_tbl, label2node, undefined_labels) =
      match instr with
      | Assem.OPER { assem; dst; src; jump } ->
          let newNode = Flow.Graph.newNode graph in
          (* Repetition of this `match` is pretty ugly *)
          (match prev_node with
          | Some prev -> Flow.Graph.mk_edge { src = prev; dst = newNode }
          | _ -> ());
          (* `next_node` so that the next instruction that we fall through to
             is aware that this node is its predecessor. *)
          let next_node, (label2node, undefined_labels) =
            match jump with
            | Some symbols ->
                (* If this is a jump, then we don't want to fall through. Hence,
                   we return `None`. *)
                ( None,
                  List.fold_left
                    (fun (label2node, undefined_labels) symbol ->
                      match Symbol.look (label2node, symbol) with
                      | Some node ->
                          Flow.Graph.mk_edge { src = newNode; dst = node };
                          (label2node, undefined_labels)
                      | None ->
                          (* If the node is not in the graph yet, create
                             it on the fly and insert it to the table. *)
                          let destNode = Flow.Graph.newNode graph in
                          Flow.Graph.mk_edge { src = newNode; dst = destNode };
                          ( Symbol.enter (label2node, symbol, destNode),
                            symbol :: undefined_labels ))
                    (label2node, undefined_labels)
                    symbols )
            | None -> (Some newNode, (label2node, undefined_labels))
          in
          ( next_node,
            Flow.Graph.Table.enter (def_tbl, newNode, dst),
            Flow.Graph.Table.enter (use_tbl, newNode, src),
            Flow.Graph.Table.enter (move_tbl, newNode, false),
            label2node,
            newNode,
            undefined_labels )
      | Assem.LABEL { assem; lab } ->
          (* Maybe the node has been created by a prior jump, hence we
             look it up. *)
          let node, undefined_labels =
            match Symbol.look (label2node, lab) with
            | Some node ->
                (* Since we are defining it here, remove it from the
                   list of undefined labels. *)
                ( node,
                  List.filter
                    (fun e -> not @@ Symbol.equal_symbol e lab)
                    undefined_labels )
            | None -> (Flow.Graph.newNode graph, undefined_labels)
          in
          (match prev_node with
          | Some prev -> Flow.Graph.mk_edge { src = prev; dst = node }
          | _ -> ());
          ( Some node,
            Flow.Graph.Table.enter (def_tbl, node, []),
            Flow.Graph.Table.enter (use_tbl, node, []),
            Flow.Graph.Table.enter (move_tbl, node, false),
            Symbol.enter (label2node, lab, node),
            node,
            undefined_labels )
      | Assem.MOVE { assem; dst; src } ->
          let newNode = Flow.Graph.newNode graph in
          (match prev_node with
          | Some prev -> Flow.Graph.mk_edge { src = prev; dst = newNode }
          | _ -> ());
          ( Some newNode,
            Flow.Graph.Table.enter (def_tbl, newNode, [ dst ]),
            Flow.Graph.Table.enter (use_tbl, newNode, [ src ]),
            Flow.Graph.Table.enter (move_tbl, newNode, true),
            label2node,
            newNode,
            undefined_labels )
    in

    let _, nodes, def_tbl, use_tbl, move_tbl, _, undefined_labels =
      List.fold_left
        (fun (prev_node, l, dtbl, utbl, mtbl, label2node, undefined_labels)
             instr ->
          let prev_node, dtbl', utbl', mtbl', label2node', l', undefined_labels
              =
            handle_instr instr
              (prev_node, dtbl, utbl, mtbl, label2node, undefined_labels)
          in
          ( prev_node,
            l' :: l,
            dtbl',
            utbl',
            mtbl',
            label2node',
            undefined_labels ))
        ( None,
          [],
          Flow.Graph.Table.empty,
          Flow.Graph.Table.empty,
          Flow.Graph.Table.empty,
          Symbol.empty,
          [] )
        instrs
    in
    match undefined_labels with
    | [] ->
        (* There should be no undefined labels! *)
        let fg =
          Flow.FGRAPH
            { control = graph; def = def_tbl; use = use_tbl; ismove = move_tbl }
        in
        (fg, List.rev nodes)
    | l :: ls ->
        let label_string =
          List.fold_left
            (fun acc sym -> acc ^ ", " ^ Symbol.name sym)
            (Symbol.name l) ls
        in
        (* Since these labels are compiler generated, this shouldn't happen.
           If it does, then it should be a compiler bug. *)
        ErrorMsg.impossible @@ "Jumped to undefined label(s): " ^ label_string
end

open Base
open Temp

let%test_unit "make_graph_simple_block" =
  (* Based on tests/test1.tig *)
  let l0 = Temp.named_label "l0" in
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

let%test_unit "make_graph_jump_before_label_definition" =
  (* I have the jump instruction defined 'before' the label, and this should work
     anyway. *)
  let l1 = Temp.named_label "L1" in
  let instrs =
    [
      Assem.LABEL { assem = "L1:\n"; lab = l1 };
      Assem.OPER { assem = "j L1\n"; dst = []; src = []; jump = Some [ l1 ] };
    ]
  in
  let _ = MakeGraph.instrs2graph instrs in
  ()

let%expect_test "make_graph_jump_to_undefined_label" =
  let open Expect_test_helpers_base in
  (* I have the jump instruction defined 'before' the label, and this should work
     anyway. *)
  let l1 = Temp.named_label "L1" in
  let instrs =
    [ Assem.OPER { assem = "j L1\n"; dst = []; src = []; jump = Some [ l1 ] } ]
  in
  require_does_raise [%here] (fun () -> MakeGraph.instrs2graph instrs);
  [%expect
    {|
    (Tiger.Errormsg.ErrorMsg.Error)
    Error: Compiler bug: Jumped to undefined label(s): L1 |}]
