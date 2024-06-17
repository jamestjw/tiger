open Base
open Graph
open Temp

module Flow = struct
  module Graph = Graph

  type flowgraph =
    | FGRAPH of {
        (* Graph where each node is an instruction or basic block *)
        control : Graph.graph;
        (* Table of defined temporaries at each node *)
        def : Temp.temp list Graph.Table.tbl;
        (* Table of used temporaries at each node *)
        use : Temp.temp list Graph.Table.tbl;
        (* Table that keeps track of whether each node is a `move` *)
        ismove : bool Graph.Table.tbl;
      }

  (* How to use:
     If there are any nonzero number of defs, mention def(x).
     If there are any nonzero number of uses BEFORE THE FIRST DEF,
         mention use(x)

     For any node in the graph,
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
  *)

  let table_get_list (tbl : 'a list Graph.Table.tbl) (node : Graph.node) =
    Graph.Table.look (tbl, node) |> Option.value ~default:[]

  let show (FGRAPH { control; def; use; _ }) : unit =
    let temp2string = Frame.Frame.register_to_string_default in
    let do_node node =
      Stdio.print_endline @@ Graph.nodename node;
      Stdio.printf "\tPred nodes : %s."
        (String.concat ~sep:" " (List.map ~f:Graph.nodename (Graph.pred node)));
      Stdio.printf "\tSucc nodes : %s\n"
        (String.concat ~sep:" " (List.map ~f:Graph.nodename (Graph.succ node)));
      Stdio.printf "\tDefs: %s."
        (String.concat ~sep:" "
           (List.map ~f:temp2string
              (Graph.Table.look (def, node) |> Option.value ~default:[])));

      Stdio.printf "\tUses: %s\n"
        (String.concat ~sep:" "
           (List.map ~f:temp2string
              (Graph.Table.look (use, node) |> Option.value ~default:[])));
      ()
    in
    List.iter ~f:do_node (Graph.nodes control)

  let get_temps (FGRAPH { control; def; use; _ }) : Temp.Set.t =
    let do_node set node =
      Temp.Set.union set (table_get_list def node |> Temp.Set.of_list)
      |> Temp.Set.union (table_get_list use node |> Temp.Set.of_list)
    in
    List.fold_left ~f:do_node ~init:Temp.Set.empty (Graph.nodes control)
end
