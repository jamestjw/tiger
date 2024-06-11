open Temp
open Base

module Assem = struct
  type reg = string
  type temp = Temp.temp [@@deriving ord, sexp, show]
  type label = Temp.label [@@deriving ord, sexp, show]

  type instr =
    | OPER of {
        assem : string;
        dst : temp list;
        src : temp list;
        jump : label list option;
      }
    | LABEL of { assem : string; lab : Temp.label }
    | MOVE of { assem : string; dst : temp; src : temp }
  [@@deriving ord, sexp, show]

  let label_to_string = Temp.label_to_string

  let format f instr =
    match instr with
    | OPER { assem; src; dst; jump } ->
        let jump' = match jump with Some l -> l | None -> [] in
        let res =
          List.fold2_exn ~init:assem
            ~f:(fun s i label ->
              Str.global_replace
                (Str.regexp ("'s" ^ Int.to_string i))
                (f label) s)
            (List.range 0 (List.length src))
            src
        in
        let res2 =
          List.fold2_exn ~init:res
            ~f:(fun s i label ->
              Str.global_replace
                (Str.regexp ("'d" ^ Int.to_string i))
                (f label) s)
            (List.range 0 (List.length dst))
            dst
        in
        List.fold2_exn ~init:res2
          ~f:(fun s i label ->
            Str.global_replace
              (Str.regexp ("'j" ^ Int.to_string i))
              (label_to_string label) s)
          (List.range 0 (List.length jump'))
          jump'
    | LABEL { assem; _ } -> assem
    | MOVE { assem; dst; src } ->
        let res = Str.global_replace (Str.regexp "'s0") (f src) assem in
        Str.global_replace (Str.regexp "'d0") (f dst) res
end
