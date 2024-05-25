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
end
