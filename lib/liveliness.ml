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
           m is moved to n, and we would prefer for them both
           to be assigned to the same register *)
        moves : (IGraph.node * IGraph.node) list;
      }

  type liveSet = unit Temp.tbl * Temp.temp list

  (* Map that keeps track of what is live at the exit of each node. The
     liveset has a redundancy
       - The table makes it easy to do membership tests
       - The list allows us to enumerate all live temporaries *)
  type liveMap = liveSet Flow.Graph.Table.tbl

  (* Need set of temporaries when building live map *)
  module TempSet = Set.Make (struct
    type t = Temp.temp

    let compare = Temp.compare_temp
  end)

  (* Pretty print the graph:
     Print list of nodes in the interference graph, and for each node,
     a list of nodes adjacent to it *)
  let show (g : igraph) : unit = failwith "not implemented"

  let mkLiveMap (g : Flow.flowgraph) (nodes : Flow.Graph.node list) : liveMap =
    let (FGRAPH { control; def; use; ismove }) = g in
    let entry_count (m : (TempSet.t * TempSet.t) Flow.Graph.Table.tbl) : int =
      List.fold_left
        (fun acc (_, (s1, s2)) ->
          acc + TempSet.cardinal s1 + TempSet.cardinal s2)
        0
        (Flow.Graph.Table.bindings m)
    in
    let rec helper (m : (TempSet.t * TempSet.t) Flow.Graph.Table.tbl) =
      let do_node m node =
        let ins, outs =
          Flow.Graph.Table.look (m, node)
          |> Option.value ~default:(TempSet.empty, TempSet.empty)
        in
        let defs =
          Flow.Graph.Table.look (def, node)
          |> Option.value ~default:[] |> TempSet.of_list
        in
        let uses =
          Flow.Graph.Table.look (use, node)
          |> Option.value ~default:[] |> TempSet.of_list
        in
        let successor_ins =
          List.map
            (fun n ->
              Flow.Graph.Table.look (m, n)
              |> Option.fold ~none:TempSet.empty ~some:(fun (ins, _) -> ins))
            (Flow.Graph.succ node)
        in
        let ins = TempSet.union uses @@ TempSet.diff outs defs in
        let outs = List.fold_left TempSet.union TempSet.empty successor_ins in
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
        let temp_list = List.of_seq @@ TempSet.to_seq outs in
        let temp_tbl =
          TempSet.fold
            (fun temp tbl -> Temp.enter (tbl, temp, ()))
            outs Temp.empty
        in
        (temp_tbl, temp_list))
      in_out_tbl

  let mkInterferenceGraph :
      Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list) =
    failwith "TODO!"
end
