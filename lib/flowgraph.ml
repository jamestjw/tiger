open Graph
open Temp

module Flow = struct
  module Graph = Graph

  type flowgraph =
    | FGRAPH of {
        control : Graph.graph;
        def : Temp.temp list Graph.Table.tbl;
        use : Temp.temp list Graph.Table.tbl;
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
