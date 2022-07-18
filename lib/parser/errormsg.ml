module type ERRORMSG = sig
  val anyErrors : bool ref
  val error : Lexing.lexbuf -> string -> unit

  exception Error

  val impossible : string -> 'a (* raises Error *)
  val reset : unit -> unit
end

module ErrorMsg = struct
  let anyErrors = ref false
  let reset () = anyErrors := false

  exception Error

  let error lexbuf (msg : string) =
    anyErrors := true;
    let pos = lexbuf.Lexing.lex_curr_p in
    print_string pos.pos_fname;
    Stdio.printf ":%d.%d: " pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    print_string msg;
    print_endline ""

  let impossible msg =
    print_string ("Error: Compiler bug: " ^ msg ^ "\n");
    raise Error
end
