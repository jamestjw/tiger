module type ERRORMSG = sig
  val anyErrors : bool ref
  val error : Lexing.lexbuf -> string -> unit
  val error_pos : int -> string -> unit
  val fileName : string ref

  exception Error

  val impossible : string -> 'a (* raises Error *)
  val reset : unit -> unit
end

module ErrorMsg = struct
  let anyErrors = ref false
  let fileName = ref ""

  let reset () =
    anyErrors := false;
    fileName := ""

  exception Error

  let error lexbuf (msg : string) =
    anyErrors := true;
    let pos = lexbuf.Lexing.lex_curr_p in
    print_string pos.pos_fname;
    Stdio.printf ":%d.%d: " pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    print_string msg;
    print_endline ""

  let error_pos pos (msg : string) =
    anyErrors := true;
    (* TODO: Fix this to actually print the right char position *)
    Stdio.printf "%s:%d.%d: %s\n" !fileName 0 pos msg

  let impossible msg =
    print_string ("Error: Compiler bug: " ^ msg ^ "\n");
    raise Error
end
