(* In this file, `k` often refers to the number of registers *)

open Assem

(* open Flowgraph *)
open Frame
open Codegen
open Color

(* open Makegraph *)
(* open Liveness *)
open Temp
open Core

module RegAlloc = struct
  module Frame = Frame

  type allocation = Frame.register Temp.tbl

  let rewrite_program spilled_nodes (instrs : Assem.instr list) frame :
      Assem.instr list =
    let accesses =
      List.fold ~init:Temp.empty
        ~f:(fun tbl n -> Temp.enter (tbl, n, Frame.alloc_local frame true))
        spilled_nodes
    in
    (* If we need to enlarge the stack, we need to decrement the stack pointer! *)
    let stack_enlargement_instr =
      Assem.OPER
        {
          src = [ Frame.sp ];
          dst = [];
          assem =
            Printf.sprintf "\taddi 's0, 's0, %d\n"
              (-Frame.word_size * List.length spilled_nodes);
          jump = None;
        }
    in
    (* Returns instructions in reverse order! So we need to flip it after
       folding *)
    let do_instr (instrs, temps) instr =
      let maybe_replace t =
        match Temp.look (accesses, t) with
        | Some access ->
            let t = Temp.new_temp () in
            (t, [ (t, access) ])
        | None -> (t, [])
      in
      let defs, uses, instr =
        match instr with
        | Assem.LABEL _ -> ([], [], instr)
        | Assem.MOVE ({ src; dst; _ } as data) ->
            let src, uses = maybe_replace src in
            let dst, defs = maybe_replace dst in

            (defs, uses, Assem.MOVE { data with src; dst })
        | Assem.OPER ({ src; dst; _ } as data) ->
            let src, uses = List.map ~f:maybe_replace src |> List.unzip in
            let dst, defs = List.map ~f:maybe_replace dst |> List.unzip in
            ( Stdlib.List.flatten defs,
              Stdlib.List.flatten uses,
              Assem.OPER { data with src; dst } )
      in
      (* Fetch before use *)
      let befores = List.map ~f:RiscVGen.generate_fetch uses in
      (* Store after definition *)
      let afters = List.map ~f:RiscVGen.generate_store defs in

      (List.concat [ afters; [ instr ]; befores; instrs ], temps)
    in

    let instrs, new_temps = List.fold ~f:do_instr ~init:([], []) instrs in

    stack_enlargement_instr :: List.rev instrs

  (* The function returns a new list of instructions as a result
     of coalescing moves. *)
  let alloc (instrs : Assem.instr list) (frame : Frame.frame) :
      Assem.instr list * allocation =
    let rec inner instrs =
      List.iter instrs ~f:(fun i -> Stdio.print_endline @@ Assem.show_instr i);
      let initial_allocation = Frame.get_temp_map () in
      let registers =
        Temp.IntMap.bindings initial_allocation |> List.map ~f:(fun (_, e) -> e)
      in
      let allocation, spills, coalesced =
        (* TODO: Use better spill cost *)
        Color.color instrs initial_allocation (fun _ -> 1) registers
      in

      if List.is_empty spills then
        let coalesced_set = Temp.Set.of_list coalesced in
        let coalesced_instrs =
          List.filter instrs ~f:(fun instr ->
              match instr with
              | Assem.MOVE { dst; src; _ }
                when Temp.Set.mem dst coalesced_set
                     || Frame.register_eq
                          (Temp.look_exn (allocation, src))
                          (Temp.look_exn (allocation, dst)) ->
                  false
              | _ -> true)
        in
        (coalesced_instrs, allocation)
      else (
        Printf.printf "Spills: %s\n"
          (String.concat ~sep:" "
             (List.map ~f:Frame.register_to_string_default spills));
        let instrs = rewrite_program spills instrs frame in

        Stdio.print_endline "New instrs: ";
        List.iter instrs ~f:(fun i -> Stdio.print_endline @@ Assem.show_instr i);
        (* ignore @@ Errormsg.ErrorMsg.impossible "stop here for now"; *)
        inner instrs)
    in

    inner instrs
end
