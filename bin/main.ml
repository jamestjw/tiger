open Tiger
open Base

let input_file = ref ""
let output_file = ref ""
let usage_msg = "tiger -o output-file input-file"

let speclist =
  [ ("-o", Stdlib.Arg.Set_string output_file, "Set output file name") ]

let add_input_file fn = input_file := fn

(* TODO: Use a more reliable library to handle this *)
let main () =
  Stdlib.Arg.parse speclist add_input_file usage_msg;
  if String.(!input_file = "") then (
    Stdio.print_string "Error: Missing input file\n";
    Stdlib.exit 1);

  let outc =
    if String.(!output_file = "") then Stdio.Out_channel.stdout
    else
      Stdio.Out_channel.create !output_file ~append:false ~fail_if_exists:false
  in

  Stdlib.Printf.fprintf outc "%s" (Driver.compile_file ~filename:!input_file);
  Stdio.Out_channel.close outc

let () = main ()
