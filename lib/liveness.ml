open Flowgraph
open Graph
open Temp

module Liveness = struct
  module IGraph = Graph

  type igraph =
    | IGRAPH of {
        (* Interference graph *)
        graph : IGraph.graph;
        (* Mapping from temporaries to graph nodes *)
        tnode : Temp.temp -> IGraph.node;
        (* Inverse of the above *)
        gtemp : IGraph.node -> Temp.temp;
        (* List of moves, i.e. given a pair (m , n), we know that
           n is moved to m, and we would prefer for them both
           to be assigned to the same register *)
        moves : (IGraph.node * IGraph.node) list;
      }

  type liveSet = unit Temp.tbl * Temp.temp list

  (* Map that keeps track of what is live at the exit of each node. The
     liveset has a redundancy
       - The table makes it easy to do membership tests
       - The list allows us to enumerate all live temporaries *)
  type liveMap = liveSet Flow.Graph.Table.tbl

  (* Pretty print the graph:
     Print list of nodes in the interference graph, and for each node,
     a list of nodes adjacent to it *)
  let show (IGRAPH { graph; tnode; gtemp; _ }) : unit =
    let do_node node =
      let temp = gtemp node in
      let adjacents = IGraph.adj node in
      Stdio.printf "%s (%s) (%d adjacent nodes):\n" (IGraph.nodename node)
        (Frame.Frame.register_to_string_default temp)
        (List.length adjacents);

      Stdio.printf "\t- %s\n"
        (String.concat " "
           (List.map
              (fun e -> gtemp e |> Frame.Frame.register_to_string_default)
              adjacents))
    in
    Stdio.print_endline "-- Interference Graph --";
    List.iter do_node (IGraph.nodes graph)

  let mkLiveMap (g : Flow.flowgraph) : liveMap =
    let (FGRAPH { control; def; use; ismove }) = g in
    let nodes = Flow.Graph.nodes control in
    let entry_count (m : (Temp.Set.t * Temp.Set.t) Flow.Graph.Table.tbl) : int =
      List.fold_left
        (fun acc (_, (s1, s2)) ->
          acc + Temp.Set.cardinal s1 + Temp.Set.cardinal s2)
        0
        (Flow.Graph.Table.bindings m)
    in
    let rec helper (m : (Temp.Set.t * Temp.Set.t) Flow.Graph.Table.tbl) =
      let do_node m node =
        let ins, outs =
          Flow.Graph.Table.look (m, node)
          |> Option.value ~default:(Temp.Set.empty, Temp.Set.empty)
        in
        let defs =
          Flow.Graph.Table.look (def, node)
          |> Option.value ~default:[] |> Temp.Set.of_list
        in
        let uses =
          Flow.Graph.Table.look (use, node)
          |> Option.value ~default:[] |> Temp.Set.of_list
        in
        let successor_ins =
          List.map
            (fun n ->
              Flow.Graph.Table.look (m, n)
              |> Option.fold ~none:Temp.Set.empty ~some:(fun (ins, _) -> ins))
            (Flow.Graph.succ node)
        in
        let ins = Temp.Set.union uses @@ Temp.Set.diff outs defs in
        let outs = List.fold_left Temp.Set.union Temp.Set.empty successor_ins in
        Flow.Graph.Table.enter (m, node, (ins, outs))
      in
      let res = List.fold_left do_node m nodes in
      (* Since we only add entries, if the count doesn't increase then
         we know that no change has occurred and that we have found the
         fix point. *)
      if entry_count m == entry_count res then res else helper res
    in
    let in_out_tbl = helper Flow.Graph.Table.empty in
    Flow.Graph.Table.map
      (fun (_, outs) ->
        let temp_list = List.of_seq @@ Temp.Set.to_seq outs in
        let temp_tbl =
          Temp.Set.fold
            (fun temp tbl -> Temp.enter (tbl, temp, ()))
            outs Temp.empty
        in
        (temp_tbl, temp_list))
      in_out_tbl

  (* Returns an interference graph along with a function that returns
     all the temporaries that are live out of a node in the flowgraph. *)
  let mk_interference_graph (fg : Flow.flowgraph) :
      igraph * (Flow.Graph.node -> Temp.temp list) =
    let (FGRAPH { control; def; use; ismove }) = fg in
    let lm = mkLiveMap fg in
    let tnode_tbl : IGraph.node Temp.tbl = Temp.empty in
    let gtemp_tbl : Temp.temp IGraph.Table.tbl = IGraph.Table.empty in
    let get_node (g, tnode_tbl, gtemp_tbl) temp =
      match Temp.look (tnode_tbl, temp) with
      | Some node -> (node, g, tnode_tbl, gtemp_tbl)
      | None ->
          let ((g, i) as node) = IGraph.newNode g in
          let tnode_tbl = Temp.enter (tnode_tbl, temp, node) in
          let gtemp_tbl = IGraph.Table.enter (gtemp_tbl, node, temp) in
          (node, g, tnode_tbl, gtemp_tbl)
    in

    let get_nodes (g, tnode_tbl, gtemp_tbl) node tbl =
      Flow.table_get_list tbl node
      |> List.fold_left
           (fun (acc, g, tn, gt) temp ->
             let node, g, tn, gt = get_node (g, tn, gt) temp in
             (node :: acc, g, tn, gt))
           ([], g, tnode_tbl, gtemp_tbl)
    in
    let live_out_fn (node : Flow.Graph.node) : Temp.temp list =
      Flow.Graph.Table.look (lm, node)
      |> Option.fold ~none:[] ~some:(fun (_, l) -> l)
    in

    let graph, tnode_tbl, gtemp_tbl, move_list =
      List.fold_left
        (fun (g, tn, gt, ml) node ->
          let live_temps = live_out_fn node in
          let defined_nodes, g, tn, gt = get_nodes (g, tn, gt) node def in
          let used_nodes, g, tn, gt = get_nodes (g, tn, gt) node use in
          let used_temps = Flow.table_get_list use node in

          let is_move =
            Flow.Graph.Table.look (ismove, node) |> Option.value ~default:false
          in
          List.fold_left
            (fun (g, tn, gt, ml) def_node ->
              let g, tn, gt =
                List.fold_left
                  (fun (g, tn, gt) live_temp ->
                    let live_node, g, tn, gt = get_node (g, tn, gt) live_temp in
                    if
                      (* We don't need to include the information that says that
                         a node interferes with itself. *)
                      not @@ IGraph.eq (def_node, live_node)
                    then
                      if (not is_move) || (not @@ List.mem live_temp used_temps)
                      then IGraph.mk_undirected_edge (def_node, live_node);
                    (g, tn, gt))
                  (g, tn, gt) live_temps
              in
              let ml =
                if is_move then
                  List.fold_left
                    (fun ml use_node -> (def_node, use_node) :: ml)
                    ml used_nodes
                else ml
              in
              (g, tn, gt, ml))
            (g, tn, gt, ml) defined_nodes)
        (IGraph.newGraph (), tnode_tbl, gtemp_tbl, [])
        (Flow.Graph.nodes control)
    in

    ( IGRAPH
        {
          graph;
          tnode = (fun temp -> Temp.look (tnode_tbl, temp) |> Option.get);
          gtemp =
            (fun node -> IGraph.Table.look (gtemp_tbl, node) |> Option.get);
          moves = move_list;
        },
      live_out_fn )

  (* Return adjacency set of the interference graph.
     If (u,v) in adj_set then (v, u) in adj_set *)
  let adj_set (IGRAPH { graph; gtemp; _ }) =
    IGraph.all_edges graph
    |> List.map (fun (e1, e2) -> (gtemp e1, gtemp e2))
    |> List.filter (fun (u, v) -> not (u = v))
    |> Temp.PairSet.of_list

  (* Given an interference graph, returns a list of temporaries that it
     interferes with. *)
  let adj_list (IGRAPH { graph; tnode; gtemp; _ } as igraph) :
      Temp.Set.t Temp.tbl =
    Temp.PairSet.fold
      (fun (t1, t2) tbl ->
        Temp.enter
          ( tbl,
            t1,
            Temp.look_default (tbl, t1, Temp.Set.empty) |> Temp.Set.add t2 ))
      (adj_set igraph) Temp.empty

  (* Given an interference graph, return the degree of a certain temporary,
     i.e. how many other temporaries it interferes with. *)
  let degree (ig : igraph) (t : Temp.temp) =
    let (IGRAPH { graph; tnode; _ }) = ig in
    (* Relies on the fact that the adjacent list contains no duplicates *)
    List.length @@ IGraph.adj (tnode t)
end

let%test_unit "interference_graph_simple_block" =
  (* Program:
          a <- 0
     L1 : b <- a + 1
          c <- c + b
          a <- b * 2
          if a < N goto L1
          return c

     Expected interference graph:
         a             b
         │             │
         │             │
         │             │
         │             │
         │             │
         └───── c ─────┘
  *)
  let a = Temp.new_temp () in
  let b = Temp.new_temp () in
  let c = Temp.new_temp () in
  let graph = Flow.Graph.newGraph () in
  let node1 = Flow.Graph.newNode graph in
  let node2 = Flow.Graph.newNode graph in
  let node3 = Flow.Graph.newNode graph in
  let node4 = Flow.Graph.newNode graph in
  let node5 = Flow.Graph.newNode graph in
  let node6 = Flow.Graph.newNode graph in
  Flow.Graph.mk_edge { src = node1; dst = node2 };
  Flow.Graph.mk_edge { src = node2; dst = node3 };
  Flow.Graph.mk_edge { src = node3; dst = node4 };
  Flow.Graph.mk_edge { src = node4; dst = node5 };
  Flow.Graph.mk_edge { src = node5; dst = node6 };
  Flow.Graph.mk_edge { src = node5; dst = node2 };
  let def =
    List.fold_left
      (fun tbl (node, l) -> Flow.Graph.Table.enter (tbl, node, l))
      Flow.Graph.Table.empty
      [
        (node1, [ a ]);
        (node2, [ b ]);
        (node3, [ c ]);
        (node4, [ a ]);
        (node5, []);
        (node6, []);
      ]
  in
  let use =
    List.fold_left
      (fun tbl (node, l) -> Flow.Graph.Table.enter (tbl, node, l))
      Flow.Graph.Table.empty
      [
        (node1, []);
        (node2, [ a ]);
        (node3, [ c; b ]);
        (node4, [ b ]);
        (node5, [ a ]);
        (node6, [ c ]);
      ]
  in
  let ismove =
    List.fold_left
      (fun tbl (node, b) -> Flow.Graph.Table.enter (tbl, node, b))
      Flow.Graph.Table.empty
      [
        (node1, true);
        (node2, true);
        (node3, true);
        (node4, true);
        (node5, false);
        (node6, false);
      ]
  in
  let flowgraph = Flow.FGRAPH { control = graph; def; use; ismove } in
  let (IGRAPH { graph; tnode; gtemp; moves } as igraph), live_out_fn =
    Liveness.mk_interference_graph flowgraph
  in
  Liveness.show igraph;
  let open Base in
  [%test_eq: bool] true
    (List.exists
       (Flow.Graph.adj @@ tnode c)
       ~f:(fun e -> Flow.Graph.eq (e, tnode a)));

  [%test_eq: bool] true
    (List.exists
       (Flow.Graph.adj @@ tnode c)
       ~f:(fun e -> Flow.Graph.eq (e, tnode b)));
  [%test_eq: bool] false
    (List.exists
       (Flow.Graph.adj @@ tnode a)
       ~f:(fun e -> Flow.Graph.eq (e, tnode b)))
