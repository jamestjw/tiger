module type ERRORMSG = sig
  val anyErrors : bool ref
  val fileName : string ref
  val lineNum : int ref
  val linePos : int list ref
  val sourceStream : in_channel ref
  val error : int -> string -> unit

  exception Error

  val impossible : string -> 'a (* raises Error *)
  val reset : unit -> unit
end

module ErrorMsg = struct
  let anyErrors = ref false
  let fileName = ref ""
  let lineNum = ref 1
  let linePos = ref [ 1 ]
  let sourceStream = ref stdin

  let reset () =
    anyErrors := false;
    fileName := "";
    lineNum := 1;
    linePos := [ 1 ];
    sourceStream := stdin

  exception Error

  let error pos (msg : string) =
    let rec look = function
      | a :: rest, n ->
          if a < pos then
            print_string (":" ^ Int.to_string n ^ "." ^ Int.to_string (pos - a))
          else look (rest, n - 1)
      | _ -> print_string "0.0"
    in

    anyErrors := true;
    print_string !fileName;
    look (!linePos, !lineNum);
    print_string ": ";
    print_string msg;
    print_endline ""

  let impossible msg =
    print_string ("Error: Compiler bug: " ^ msg ^ "\n");
    raise Error
end
