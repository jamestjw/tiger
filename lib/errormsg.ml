module type ERRORMSG = sig
  val anyErrors : bool ref
  val line_num : int ref
  val line_pos : int list ref
  val error : Lexing.lexbuf -> string -> unit
  val error_pos : int -> string -> unit
  val fileName : string ref
  val set_filename : string -> unit

  exception Error

  val impossible : string -> 'a (* raises Error *)
  val reset : unit -> unit
end

module ErrorMsg : ERRORMSG = struct
  let anyErrors = ref false
  let fileName = ref ""
  let line_num = ref 1
  let line_pos = ref [ 0 ]
  let set_filename filename = fileName := filename

  let reset () =
    anyErrors := false;
    fileName := "";
    line_num := 1;
    line_pos := [ 0 ]

  exception Error

  let error lexbuf (msg : string) =
    anyErrors := true;
    let pos = lexbuf.Lexing.lex_curr_p in
    print_string pos.pos_fname;
    Stdio.printf ":%d.%d: " pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    print_string msg;
    print_endline ""

  let error_pos pos (msg : string) =
    let rec look = function
      | a :: rest, n ->
          if a < pos then Int.to_string n ^ "." ^ Int.to_string (pos - a)
          else look (rest, n - 1)
      | _ -> "0.0"
    in

    anyErrors := true;
    (* TODO: Fix wrong line*)
    (* List.iter (Stdio.printf "%d ") !line_pos;Stdio.printf "\n"; *)
    Stdio.printf "%s:%s: %s\n" !fileName (look (!line_pos, !line_num)) msg

  let impossible msg =
    print_string ("Error: Compiler bug: " ^ msg ^ "\n");
    raise Error
end
