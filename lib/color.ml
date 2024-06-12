open Assem
open Core
open Frame
open Flowgraph
open Makegraph
open Liveness
open Temp

module Color = struct
  module Frame = Frame

  type allocation = Frame.register Temp.tbl

  (* Doubly linked list for efficient adding and deletion *)
  type node_list = Flow.Graph.node Doubly_linked.t
  type temp_list = Temp.temp Doubly_linked.t

  type node_location =
    | INITIAL
    | PRECOLORED
    | SIMP_WL
    | FREEZE_WL
    | SPILL_WL
    | SPILLED
    | COALESCED
    | COLORED
    | SELECT
  [@@deriving show]

  type temp_ptr = Temp.temp Doubly_linked.Elt.t

  type move_location = COALESCED | CONSTRAINED | FROZEN | WORKLIST | ACTIVE
  [@@deriving show]

  type move_ptr = Liveness.IGraph.node Doubly_linked.Elt.t

  type state = {
    (* Low-degree non-move-related nodes *)
    simplify_worklist : temp_list;
    (* Low-degree move-related nodes *)
    freeze_worklist : temp_list;
    (* High-degree nodes *)
    spill_worklist : temp_list;
    spilled_nodes : temp_list;
    coalesced_nodes : temp_list;
    colored_nodes : temp_list;
    select_stack : Temp.temp Stack.t;
    coalesced_moves : node_list;
    constrained_moves : node_list;
    frozen_moves : node_list;
    (* Move instructions that might be coalesceable *)
    worklist_moves : node_list;
    active_moves : node_list;
    move_list : Flow.Graph.NodeSet.t Temp.tbl;
    move_src_dest : (Temp.temp * Temp.temp) Flow.Graph.Table.tbl;
    adj_set : Temp.PairSet.t;
    adj_list : Temp.Set.t Temp.tbl;
    degree : int Temp.tbl;
    node_location : (node_location * temp_ptr option) Temp.tbl;
    move_location : (move_location * move_ptr) Flow.Graph.Table.tbl;
    alias : Temp.temp Temp.tbl;
  }

  let mk_empty_state (_ : unit) : state =
    {
      simplify_worklist = Doubly_linked.create ();
      freeze_worklist = Doubly_linked.create ();
      spill_worklist = Doubly_linked.create ();
      spilled_nodes = Doubly_linked.create ();
      coalesced_nodes = Doubly_linked.create ();
      colored_nodes = Doubly_linked.create ();
      select_stack = Stack.create ();
      coalesced_moves = Doubly_linked.create ();
      constrained_moves = Doubly_linked.create ();
      frozen_moves = Doubly_linked.create ();
      worklist_moves = Doubly_linked.create ();
      active_moves = Doubly_linked.create ();
      move_list = Temp.empty;
      move_src_dest = Flow.Graph.Table.empty;
      adj_list = Temp.empty;
      adj_set = Temp.PairSet.empty;
      degree = Temp.empty;
      node_location = Temp.empty;
      move_location = Liveness.IGraph.Table.empty;
      alias = Temp.empty;
    }

  exception Invalid_node
  exception Fatal_error

  let temp_tbl_look_exn tbl temp =
    match Temp.look (tbl, temp) with
    | Some e -> e
    | None ->
        Stdio.printf "Temp %s not found\n"
          (Frame.register_to_string_default temp);
        raise Invalid_node

  let get_node_location { node_location; _ } temp =
    temp_tbl_look_exn node_location temp

  let is_precolored { node_location; _ } t =
    match Temp.look (node_location, t) with
    | Some (PRECOLORED, _) -> true
    | _ -> false

  let add_edge ({ adj_set; adj_list; degree; node_location; _ } as state) (u, v)
      =
    if (not @@ Temp.PairSet.mem (u, v) adj_set) && (not @@ Temp.eq u v) then
      let adj_set =
        adj_set |> Temp.PairSet.add (u, v) |> Temp.PairSet.add (v, u)
      in
      let adj_list, degree =
        if not @@ is_precolored state u then
          ( Temp.enter
              ( adj_list,
                u,
                Temp.look_default (adj_list, u, Temp.Set.empty)
                |> Temp.Set.add v ),
            Temp.enter (degree, u, Temp.look_default (degree, u, 0) + 1) )
        else (adj_list, degree)
      in

      let adj_list, degree =
        if not @@ is_precolored state v then
          ( Temp.enter
              ( adj_list,
                v,
                Temp.look_default (adj_list, v, Temp.Set.empty)
                |> Temp.Set.add u ),
            Temp.enter (degree, v, Temp.look_default (degree, v, 0) + 1) )
        else (adj_list, degree)
      in
      { state with adj_set; adj_list; degree }
    else state

  let build (Flow.FGRAPH { control; use; ismove; def })
      (precolored : allocation) (igraph : Liveness.igraph)
      (live_out_fn : Flow.Graph.node -> Temp.temp list) : state =
    let node_location =
      List.fold ~init:Temp.empty
        ~f:(fun tbl (temp, _) -> Temp.enter (tbl, temp, (PRECOLORED, None)))
        (Temp.IntMap.bindings precolored)
    in
    let do_instr
        ({ move_list; move_src_dest; worklist_moves; move_location; _ } as state)
        node =
      let def = Temp.Set.of_list @@ Flow.Graph.Table.look_exn (def, node) in
      let use = Temp.Set.of_list @@ Flow.Graph.Table.look_exn (use, node) in
      (* Check if node is a move *)
      if Flow.Graph.Table.look_exn (ismove, node) then
        let src, dest =
          match
            ( Temp.Set.to_seq use |> Stdlib.List.of_seq,
              Temp.Set.to_seq def |> Stdlib.List.of_seq )
          with
          | [ e1 ], [ e2 ] -> (e1, e2) (* NOTE: The two could be the same. *)
          | _ -> raise Invalid_node
        in
        let move_list =
          Temp.Set.fold
            (fun n move_list ->
              let l =
                Temp.look (move_list, n)
                |> Option.value ~default:Flow.Graph.NodeSet.empty
              in
              let l = Flow.Graph.NodeSet.add node l in
              Temp.enter (move_list, n, l))
            (Temp.Set.union def use) move_list
        in
        let ptr = Doubly_linked.insert_last worklist_moves node in
        {
          state with
          move_list;
          move_location =
            Flow.Graph.Table.enter (move_location, node, (WORKLIST, ptr));
          move_src_dest =
            Flow.Graph.Table.enter (move_src_dest, node, (src, dest));
        }
      else state
    in

    (* The order doesn't matter because we do not populate `adj_set` and
       `adj_list` here. *)
    let state =
      List.fold (Flow.Graph.nodes control)
        ~init:{ (mk_empty_state ()) with node_location }
        ~f:do_instr
    in
    let adj_list = Liveness.adj_list igraph in
    let degree =
      Temp.IntMap.bindings adj_list
      |> List.fold ~init:Temp.empty ~f:(fun tbl (temp, set) ->
             Temp.enter (tbl, temp, Temp.Set.cardinal set))
    in
    { state with adj_set = Liveness.adj_set igraph; adj_list; degree }

  let node_moves { move_list; active_moves; worklist_moves; _ } temp =
    let moves = Temp.look_default (move_list, temp, Flow.Graph.NodeSet.empty) in
    let active_moves = Doubly_linked.to_list active_moves in
    let worklist_moves = Doubly_linked.to_list worklist_moves in

    Flow.Graph.NodeSet.inter moves
    @@ Flow.Graph.NodeSet.union
         (Flow.Graph.NodeSet.of_list active_moves)
         (Flow.Graph.NodeSet.of_list worklist_moves)
    |> Flow.Graph.NodeSet.to_seq |> Stdlib.List.of_seq

  let is_move_related state temp = not (List.is_empty @@ node_moves state temp)

  let adjacent { adj_list; select_stack; coalesced_nodes; _ } n =
    let to_remove =
      Stack.to_list select_stack @ Doubly_linked.to_list coalesced_nodes
    in
    Temp.Set.diff
      (Temp.look_default (adj_list, n, Temp.Set.empty))
      (Temp.Set.of_list to_remove)
    |> Temp.Set.to_seq |> Stdlib.List.of_seq

  let get_degree { degree; node_location; _ } n =
    match temp_tbl_look_exn node_location n with
    | PRECOLORED, _ -> Int.max_value
    | _ -> temp_tbl_look_exn degree n

  let enable_moves state nodes =
    List.fold nodes ~init:state ~f:(fun state n ->
        List.fold (node_moves state n) ~init:state
          ~f:(fun
              ({ move_location; active_moves; worklist_moves; _ } as state) m ->
            match Liveness.IGraph.Table.look_exn (move_location, m) with
            | ACTIVE, ptr ->
                Doubly_linked.remove active_moves ptr;

                let new_ptr = Doubly_linked.insert_last worklist_moves m in
                {
                  state with
                  move_location =
                    Liveness.IGraph.Table.enter
                      (move_location, m, (WORKLIST, new_ptr));
                }
            | _ -> state))

  let decrement_degree ({ degree; _ } as state) m k =
    let d = get_degree state m in

    let state = { state with degree = Temp.enter (degree, m, d - 1) } in
    if d = k then (
      let ({
             node_location;
             spill_worklist;
             freeze_worklist;
             simplify_worklist;
             _;
           } as state) =
        (* We do this in case `m` is adjacent to itself. *)
        let inp =
          let tmp = adjacent state m in
          if List.mem tmp m ~equal:Temp.eq then tmp else m :: tmp
        in
        enable_moves state inp
      in
      match get_node_location state m with
      (* Maybe it is already frozen ?! *)
      | FREEZE_WL, _ when is_move_related state m -> state
      | SPILL_WL, Some ptr ->
          Doubly_linked.remove spill_worklist ptr;
          let new_loc =
            if is_move_related state m then
              (FREEZE_WL, Some (Doubly_linked.insert_last freeze_worklist m))
            else (SIMP_WL, Some (Doubly_linked.insert_last simplify_worklist m))
          in
          { state with node_location = Temp.enter (node_location, m, new_loc) }
      | loc, _ ->
          Stdio.printf "Node location for %s at %s\n"
            (Frame.register_to_string_default m)
            (show_node_location loc);
          raise Invalid_node (* Fatal error! *))
    else state

  (* K is the number of registers that we have *)
  let make_worklist state (initial : Temp.temp list) (k : int) : state =
    let do_temp
        ({
           degree;
           spill_worklist;
           freeze_worklist;
           simplify_worklist;
           node_location;
           _;
         } as state) n =
      let loc, ptr =
        if Temp.look_default (degree, n, 0) >= k then
          (SPILL_WL, Doubly_linked.insert_last spill_worklist n)
        else if is_move_related state n then
          (FREEZE_WL, Doubly_linked.insert_last freeze_worklist n)
        else (SIMP_WL, Doubly_linked.insert_last simplify_worklist n)
      in

      {
        state with
        node_location = Temp.enter (node_location, n, (loc, Some ptr));
      }
    in
    List.fold ~init:state ~f:do_temp initial

  let simplify ({ simplify_worklist; select_stack; node_location; _ } as state)
      k =
    let n = Doubly_linked.remove_last simplify_worklist |> Stdlib.Option.get in
    match temp_tbl_look_exn node_location n with
    | SIMP_WL, _ ->
        Stack.push select_stack n;
        List.fold
          ~init:
            {
              state with
              node_location = Temp.enter (node_location, n, (SELECT, None));
            }
          ~f:(fun state m -> decrement_degree state m k)
          (adjacent state n)
    | _ -> raise Invalid_node

  let rec get_alias ({ node_location; alias; _ } as state) n =
    match temp_tbl_look_exn node_location n with
    | COALESCED, _ -> get_alias state @@ temp_tbl_look_exn alias n
    | _ -> n

  let coalesce state k =
    let add_worklist
        ({ node_location; freeze_worklist; simplify_worklist; _ } as state) u =
      let d = get_degree state u in
      if
        (not (is_precolored state u))
        && (not (is_move_related state u))
        && d < k
      then
        match temp_tbl_look_exn node_location u with
        | FREEZE_WL, Some ptr ->
            let new_ptr = Doubly_linked.insert_last simplify_worklist u in
            Doubly_linked.remove freeze_worklist ptr;
            {
              state with
              node_location =
                Temp.enter (node_location, u, (SIMP_WL, Some new_ptr));
            }
        | _ -> raise Invalid_node
      else state
    in
    let ok ({ degree; adj_set; _ } as state) t r =
      get_degree state t < k
      || is_precolored state t
      || Temp.PairSet.mem (t, r) adj_set
    in
    let conservative state nodes =
      List.filter nodes ~f:(fun n -> get_degree state n >= k) |> List.length < k
    in
    let combine
        ({
           node_location;
           freeze_worklist;
           spill_worklist;
           coalesced_nodes;
           alias;
           move_list;
           _;
         } as state) u v =
      (match temp_tbl_look_exn node_location v with
      | FREEZE_WL, Some ptr -> Doubly_linked.remove freeze_worklist ptr
      | SPILL_WL, Some ptr -> Doubly_linked.remove spill_worklist ptr
      | _ -> raise Invalid_node);

      let new_ptr = Doubly_linked.insert_last coalesced_nodes v in

      let move_list_u =
        Temp.look_default (move_list, u, Flow.Graph.NodeSet.empty)
      in
      let move_list_v =
        Temp.look_default (move_list, v, Flow.Graph.NodeSet.empty)
      in

      let state =
        {
          state with
          alias = Temp.enter (alias, v, u);
          node_location =
            Temp.enter (node_location, v, (COALESCED, Some new_ptr));
          move_list =
            Temp.enter
              (move_list, u, Flow.Graph.NodeSet.union move_list_u move_list_v);
        }
      in
      let state = enable_moves state [ v ] in

      let ({ node_location; freeze_worklist; spill_worklist; _ } as state) =
        List.fold (adjacent state v) ~init:state ~f:(fun state t ->
            let state' = add_edge state (t, u) in
            decrement_degree state' t k)
      in
      let u_degree = get_degree state u in
      match (temp_tbl_look_exn node_location u, u_degree >= k) with
      | (FREEZE_WL, Some ptr), true ->
          Doubly_linked.remove freeze_worklist ptr;
          let new_ptr = Doubly_linked.insert_last spill_worklist u in
          {
            state with
            node_location =
              Temp.enter (node_location, u, (SPILL_WL, Some new_ptr));
          }
      | (FREEZE_WL, None), true -> raise Invalid_node (* Fatal error *)
      | _ -> state
    in

    (* Main body of `coalesce` function  *)
    let {
      worklist_moves;
      move_src_dest;
      node_location;
      move_location;
      active_moves;
      coalesced_moves;
      constrained_moves;
      adj_set;
      _;
    } =
      state
    in

    let m = Doubly_linked.remove_first worklist_moves |> Stdlib.Option.get in
    (* Checking that m was indeed in the right `move_location`. *)
    (match Flow.Graph.Table.look_exn (move_location, m) with
    | WORKLIST, _ -> ()
    | _ -> raise Invalid_node);

    let move_src, move_dest = Flow.Graph.Table.look_exn (move_src_dest, m) in
    let move_src = get_alias state move_src in
    let move_dest = get_alias state move_dest in

    let u, v =
      match temp_tbl_look_exn node_location move_dest with
      | PRECOLORED, _ -> (move_dest, move_src)
      | _ -> (move_src, move_dest)
    in

    if Temp.eq u v then
      let new_ptr = Doubly_linked.insert_last coalesced_moves m in
      let state =
        {
          state with
          move_location =
            Flow.Graph.Table.enter (move_location, m, (COALESCED, new_ptr));
        }
      in

      add_worklist state u
    else if is_precolored state v || Temp.PairSet.mem (u, v) adj_set then
      let new_ptr = Doubly_linked.insert_last constrained_moves m in
      let state =
        {
          state with
          move_location =
            Flow.Graph.Table.enter (move_location, m, (CONSTRAINED, new_ptr));
        }
      in

      add_worklist state u |> Fn.flip add_worklist v
    else if
      is_precolored state u
      && List.for_all (adjacent state v) ~f:(fun t -> ok state t u)
      || (not @@ is_precolored state u)
         && conservative state
              (List.dedup_and_sort ~compare:Temp.compare_temp
                 (adjacent state u @ adjacent state v))
    then
      let new_ptr = Doubly_linked.insert_last coalesced_moves m in
      let state =
        {
          state with
          move_location =
            Flow.Graph.Table.enter (move_location, m, (COALESCED, new_ptr));
        }
      in
      combine state u v |> Fn.flip add_worklist u
    else
      let new_ptr = Doubly_linked.insert_last active_moves m in

      {
        state with
        move_location =
          Flow.Graph.Table.enter (move_location, m, (ACTIVE, new_ptr));
      }

  let freeze_moves state u k =
    let do_move
        ({
           move_location;
           move_src_dest;
           alias;
           node_location;
           active_moves;
           frozen_moves;
           degree;
           _;
         } as state) m =
      let x, y = Flow.Graph.Table.look_exn (move_src_dest, m) in
      let alias_u = get_alias state u in
      let alias_y = get_alias state y in
      let v = if Temp.eq alias_y alias_u then get_alias state x else alias_y in
      match Flow.Graph.Table.look_exn (move_location, m) with
      | ACTIVE, ptr ->
          Doubly_linked.remove active_moves ptr;
          let new_ptr = Doubly_linked.insert_last frozen_moves m in
          let state =
            {
              state with
              move_location =
                Flow.Graph.Table.enter (move_location, m, (FROZEN, new_ptr));
            }
          in
          if List.is_empty (node_moves state v) && get_degree state v < k then
            let { node_location; freeze_worklist; simplify_worklist; _ } =
              state
            in
            match Temp.look (node_location, v) with
            | Some (FREEZE_WL, Some ptr) ->
                Doubly_linked.remove freeze_worklist ptr;
                let new_loc =
                  (SIMP_WL, Some (Doubly_linked.insert_last simplify_worklist v))
                in
                {
                  state with
                  node_location = Temp.enter (node_location, v, new_loc);
                }
            | _ -> raise Invalid_node
          else state
      | loc, _ -> raise Invalid_node
    in
    List.fold ~init:state ~f:do_move (node_moves state u)

  (* This should be called when freeze_worklist != ∅ *)
  let freeze ({ freeze_worklist; simplify_worklist; node_location; _ } as state)
      k =
    let u = Doubly_linked.remove_first freeze_worklist |> Stdlib.Option.get in
    match Temp.look (node_location, u) with
    | Some (FREEZE_WL, Some ptr) ->
        let new_loc =
          (SIMP_WL, Some (Doubly_linked.insert_last simplify_worklist u))
        in
        freeze_moves
          { state with node_location = Temp.enter (node_location, u, new_loc) }
          u k
    | _ -> raise Invalid_node

  let select_spill
      ({ spill_worklist; node_location; simplify_worklist; _ } as state)
      (spill_cost : Temp.temp -> int) k =
    let spills =
      Doubly_linked.to_list spill_worklist
      |> List.sort ~compare:(fun e1 e2 ->
             compare (spill_cost e1) (spill_cost e2))
    in
    match spills with
    | m :: _ -> (
        match temp_tbl_look_exn node_location m with
        | SPILL_WL, Some ptr ->
            Doubly_linked.remove spill_worklist ptr;
            let new_ptr = Doubly_linked.insert_last simplify_worklist m in
            freeze_moves
              {
                state with
                node_location =
                  Temp.enter (node_location, m, (SIMP_WL, Some new_ptr));
              }
              m k
        | _ -> raise Invalid_node)
    (* This should never happen, we should always have something to spill when
       we call this function *)
    | _ -> raise Fatal_error

  let assign_colors ({ select_stack; _ } as state) (initial : allocation)
      (registers : Frame.register list) : state * allocation =
    let do_select
        ( tbl,
          ({ adj_list; node_location; spilled_nodes; colored_nodes; _ } as state)
        ) n =
      let filter_register w registers =
        let alias_w = get_alias state w in
        match temp_tbl_look_exn node_location alias_w with
        | COLORED, _ | PRECOLORED, _ ->
            let w_color = temp_tbl_look_exn tbl alias_w in
            (* Remove colors already taken by neighbours *)
            List.filter registers ~f:(fun e ->
                not @@ Frame.register_eq e w_color)
        | _ -> registers
      in
      let registers =
        Temp.Set.fold filter_register
          (Temp.look_default (adj_list, n, Temp.Set.empty))
          registers
      in
      match registers with
      | [] ->
          ( tbl,
            {
              state with
              node_location =
                Temp.enter
                  ( node_location,
                    n,
                    (SPILLED, Some (Doubly_linked.insert_last spilled_nodes n))
                  );
            } )
      | c :: _ ->
          ( Temp.enter (tbl, n, c),
            {
              state with
              node_location =
                Temp.enter
                  ( node_location,
                    n,
                    (COLORED, Some (Doubly_linked.insert_last colored_nodes n))
                  );
            } )
    in
    let color_tbl, state =
      Stack.to_list select_stack
      |> List.fold ~init:(initial, state) ~f:do_select
    in
    let { coalesced_nodes; _ } = state in
    Stack.clear select_stack;
    let final_tbl =
      Doubly_linked.fold coalesced_nodes ~init:color_tbl ~f:(fun tbl n ->
          Temp.enter (tbl, n, temp_tbl_look_exn tbl (get_alias state n)))
    in
    (state, final_tbl)

  exception Degree_invariant_violation
  exception Simplify_wl_invariant_violation

  let check_invariants
      {
        simplify_worklist;
        freeze_worklist;
        spill_worklist;
        degree;
        adj_list;
        move_list;
        active_moves;
        worklist_moves;
        _;
      } precolored k =
    let simp_freeze_spill =
      Temp.Set.union
        (Doubly_linked.to_list simplify_worklist |> Temp.Set.of_list)
        (Doubly_linked.to_list freeze_worklist |> Temp.Set.of_list)
      |> Temp.Set.union
           (Doubly_linked.to_list spill_worklist |> Temp.Set.of_list)
    in

    (* Degree invariant *)
    Temp.Set.iter
      (fun u ->
        let adj = Temp.look_default (adj_list, u, Temp.Set.empty) in
        let precolored_simp_freeze_spill =
          Temp.Set.union (Temp.Set.of_list precolored) simp_freeze_spill
        in
        let neighbors = Temp.Set.inter adj precolored_simp_freeze_spill in
        let u_degree = temp_tbl_look_exn degree u in
        let num_neighbors = Temp.Set.cardinal neighbors in
        if u_degree = num_neighbors then ()
        else raise Degree_invariant_violation)
      simp_freeze_spill;

    (* Simplify worklist invariant *)
    Doubly_linked.iter simplify_worklist ~f:(fun u ->
        let move_list =
          Temp.look_default (move_list, u, Flow.Graph.NodeSet.empty)
        in
        let u_degree = temp_tbl_look_exn degree u in
        let cond1 = u_degree < k in
        let cond2 =
          Flow.Graph.NodeSet.cardinal
            (Flow.Graph.NodeSet.inter move_list
               (Flow.Graph.NodeSet.of_list
                  (Doubly_linked.to_list active_moves
                  @ Doubly_linked.to_list worklist_moves)))
          = 0
        in
        match (cond1, cond2) with
        | false, _ -> raise Simplify_wl_invariant_violation
        | _, false -> raise Simplify_wl_invariant_violation
        | _ -> ());

    (* Freeze worklist invariant *)
    Doubly_linked.iter freeze_worklist ~f:(fun u ->
        let move_list =
          Temp.look_default (move_list, u, Flow.Graph.NodeSet.empty)
        in
        let cond1 = temp_tbl_look_exn degree u < k in
        let cond2 =
          not
            (Flow.Graph.NodeSet.cardinal
               (Flow.Graph.NodeSet.inter move_list
                  (Flow.Graph.NodeSet.of_list
                     (Doubly_linked.to_list active_moves
                     @ Doubly_linked.to_list worklist_moves)))
            = 0)
        in
        assert (cond1 && cond2));

    (* Spill worklist invariant *)
    Doubly_linked.iter spill_worklist ~f:(fun u ->
        assert (temp_tbl_look_exn degree u >= k))

  (* The function produces an extension of the initial allocation by
     assigning all temporaries in the interference graph a register
     from the registers list. It also returns a list of spills and
     coalesced nodes. *)
  let color (instrs : Assem.instr list)
      (* (interference : Liveness.igraph) *)
      (* Precoloring of temporaries imposed by calling conventions *)
        (initial : allocation)
      (* The cost of spilling each temporary. The simplest implementation
         that works is a function that always returns 1. *)
        (spill_cost : Temp.temp -> int)
      (* List of registers/colors *)
        (registers : Frame.register list) : allocation * Temp.temp list =
    (* Num registers is represented by the variable `k` here and in the
       other helper functions. *)
    let k = List.length registers in
    let precolored =
      Temp.IntMap.bindings initial |> List.map ~f:(fun (e, _) -> e)
    in
    (* The `repeat...until` in the book *)
    let rec repeat
        ({
           simplify_worklist;
           worklist_moves;
           freeze_worklist;
           spill_worklist;
           _;
         } as state) =
      if not @@ Doubly_linked.is_empty simplify_worklist then
        simplify state k |> repeat
      else if not @@ Doubly_linked.is_empty worklist_moves then
        coalesce state k |> repeat
      else if not @@ Doubly_linked.is_empty freeze_worklist then
        freeze state k |> repeat
      else if not @@ Doubly_linked.is_empty spill_worklist then
        select_spill state spill_cost k |> repeat
      else state (* All worklists are empty, this means that we are done *)
    in

    let flowgraph, flowgraph_nodes = MakeGraph.instrs2graph instrs in
    let (IGRAPH { graph; gtemp; _ } as igraph), live_out_fn =
      Liveness.mk_interference_graph flowgraph
    in

    (* Filter out temps that are not precolored *)
    let initial_temps =
      Liveness.IGraph.nodes graph
      |> List.map ~f:gtemp
      |> List.filter ~f:(fun temp ->
             Temp.look (initial, temp) |> Option.is_none)
    in

    let state = build flowgraph initial igraph live_out_fn in
    check_invariants state precolored k;
    let state = make_worklist state initial_temps k |> repeat in
    let ({ spilled_nodes; coalesced_nodes; _ } as state), allocation =
      assign_colors state initial registers
    in
    (allocation, Doubly_linked.to_list spilled_nodes)
end
