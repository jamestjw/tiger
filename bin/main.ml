open Tiger
open Core

let command =
  Command.basic
    ~summary:
      "Compiles a Tiger file to RISC-V assembly.\n\
       Usage: tiger input-file -o output-file"
    (let%map_open.Command input_file =
       anon ("filename" %: Filename_unix.arg_type)
     and output_file =
       flag "-o" (optional string)
         ~doc:"output-filename Output RISC-V assembly file."
     in
     fun () ->
       let outc =
         match output_file with
         | Some output_file ->
             Stdio.Out_channel.create output_file ~append:false
               ~fail_if_exists:false
         | None -> Stdio.Out_channel.stdout
       in
       Stdlib.Printf.fprintf outc "%s"
         (Driver.compile_file ~filename:input_file);
       Stdio.Out_channel.close outc)

let () = Command_unix.run command
