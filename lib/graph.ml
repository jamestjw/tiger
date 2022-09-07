open Base
open Table

module type GRAPH = sig
  type graph
  type node
  type edge = { src : node; dst : node }

  val nodes : graph -> node list
  val succ : node -> node list
  val pred : node -> node list
  val adj : node -> node list (* succ+pred *)
  val eq : node * node -> bool
  val newGraph : unit -> graph
  val newNode : graph -> node

  exception GraphEdge

  val mk_edge : edge -> unit
  val rm_edge : edge -> unit
  val nodename : node -> string (* for debugging only *)

  module Table : TABLE
end

module Graph : GRAPH = struct
  type node' = int

  (* type temp = Temp.temp *)
  type noderep = NODE of { succ : node' list; pred : node' list }

  let emptyNode = NODE { succ = []; pred = [] }
  let bogusNode = NODE { succ = [ -1 ]; pred = [] }
  let isBogus = function NODE { succ = -1 :: _; _ } -> true | _ -> false

  type graph = noderep BatDynArray.t
  type node = graph * node'

  let eq ((_, a), (_, b)) = a = b
  let augment (g : graph) (n : node') : node = (g, n)
  let newGraph () = BatDynArray.singleton bogusNode

  let nodes g =
    let rec f i =
      if isBogus (BatDynArray.get g i) then [] else (g, i) :: f (i + 1)
    in
    f 0

  let succ (g, i) =
    let (NODE { succ = s; _ }) = BatDynArray.get g i in
    List.map ~f:(augment g) s

  let pred (g, i) =
    let (NODE { pred = p; _ }) = BatDynArray.get g i in
    List.map ~f:(augment g) p

  let adj gi = pred gi @ succ gi

  let newNode g =
    (* Append node at the end *)
    let i = BatDynArray.length g in
    BatDynArray.add g emptyNode;
    (g, i)

  exception GraphEdge

  let check (_g, _g') =
    (* if Poly.(g = g') then () else raise GraphEdge *)
    ()

  (* Raises an exception if we try to delete something that
     isn't in the graph *)
  let rec delete = function
    | i, j :: rest -> if i = j then rest else j :: delete (i, rest)
    | _, [] -> raise GraphEdge

  type edge = { src : node; dst : node }

  let diddle_edge change { src = g, i; dst = g', j } =
    let _ = check (g, g') in
    let (NODE { succ = si; pred = pi }) = BatDynArray.get g i in
    BatDynArray.set g i (NODE { succ = change (j, si); pred = pi });
    let (NODE { succ = sj; pred = pj }) = BatDynArray.get g j in
    BatDynArray.set g j (NODE { succ = sj; pred = change (i, pj) })

  let mk_edge = diddle_edge (fun (x, l) -> x :: l)
  let rm_edge = diddle_edge delete
  let nodename (_g, i) = "n" ^ Int.to_string i

  module Table = Table.IntMapTable (struct
    type t = node

    let getKey (_, i) = i
  end)
end

let%test_unit "equal_nodes" =
  let graph = Graph.newGraph () in
  let node1 = Graph.newNode graph in
  let node2 = Graph.newNode graph in
  [%test_eq: bool] true (Graph.eq (node1, node1));
  [%test_eq: bool] true (Graph.eq (node2, node2));
  [%test_eq: bool] false (Graph.eq (node1, node2))

let%test_unit "adjacent_nodes" =
  let graph = Graph.newGraph () in
  let node1 = Graph.newNode graph in
  let node2 = Graph.newNode graph in
  Graph.mk_edge { src = node1; dst = node2 };
  (* Check if node1 and node2 are adjacent to each other *)
  [%test_eq: bool] true
    (Option.is_some
       (List.find (Graph.adj node1) ~f:(fun n -> Graph.eq (n, node2))));
  [%test_eq: bool] true
    (Option.is_some
       (List.find (Graph.adj node2) ~f:(fun n -> Graph.eq (n, node1))));
  (* Check if node2 is a succ of node1 *)
  [%test_eq: bool] true
    (Option.is_some
       (List.find (Graph.succ node1) ~f:(fun n -> Graph.eq (n, node2))));
  (* Check if node1 is a pred of node2 *)
  [%test_eq: bool] true
    (Option.is_some
       (List.find (Graph.pred node2) ~f:(fun n -> Graph.eq (n, node1))));
  Graph.rm_edge { src = node1; dst = node2 };
  (* Check if node1 and node2 are no longer adjacent to each other *)
  [%test_eq: bool] false
    (Option.is_some
       (List.find (Graph.adj node1) ~f:(fun n -> Graph.eq (n, node2))));
  [%test_eq: bool] false
    (Option.is_some
       (List.find (Graph.adj node2) ~f:(fun n -> Graph.eq (n, node1))))
