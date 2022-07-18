module type ERRORMSG = sig
  val anyErrors : bool ref
  val fileName : string ref
  val error : Lexing.lexbuf -> string -> unit

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
    print_string !fileName;
    let pos = lexbuf.Lexing.lex_curr_p in
    (* +1 because we want to print with 1-index *)
    Stdio.printf ":%d.%d: " pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
    print_string msg;
    print_endline ""

  let impossible msg =
    print_string ("Error: Compiler bug: " ^ msg ^ "\n");
    raise Error
end
