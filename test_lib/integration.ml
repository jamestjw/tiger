(* Integration tests, compile and execute test files and compare them to
   expected output. *)

open Core
open Tiger

let%expect_test _ =
  print_s [%sexp (List.rev [ 3; 2; 1 ] : int list)];
  [%expect {| (1 2 3) |}]

let%test_unit _ =
  let sanitise =
    (* For some reason, `spike pk` inserts this null character? *)
    Str.global_replace (Str.regexp "\000") ""
  in
  let test_dir = "../../../tests/integration/" in
  let runtime_file = "../../../btl/runtime.c" in
  let input_dir = "input/" in
  let expected_dir = "expected/" in
  let encountered_err = ref false in
  let do_file fname =
    Stdio.printf "Testing %s - " fname;
    let output_asm_fname = Stdlib.Filename.chop_extension fname ^ ".s" in
    let output_bin_fname = Stdlib.Filename.chop_extension fname ^ ".out" in
    let full_fname = test_dir ^ input_dir ^ fname in
    let expected_fname =
      test_dir ^ expected_dir ^ Stdlib.Filename.chop_extension fname ^ ".output"
    in
    let expected =
      In_channel.with_file ~f:In_channel.input_all expected_fname
    in
    let output = Driver.compile_file ~filename:full_fname in
    let outc =
      Stdio.Out_channel.create output_asm_fname ~append:false
        ~fail_if_exists:false
    in
    Printf.fprintf outc "%s" output;
    Stdio.Out_channel.flush outc;
    let cmd =
      Printf.sprintf
        "riscv64-unknown-elf-gcc %s %s -Wl,--wrap=getchar,--wrap=strcmp -o %s \
         && spike pk %s"
        output_asm_fname runtime_file output_bin_fname output_bin_fname
    in

    let cmd_inc, cmd_outc = Core_unix.open_process cmd in
    let cmd_output =
      In_channel.input_lines ~fix_win_eol:true cmd_inc
      |> List.tl |> Stdlib.Option.get |> String.concat ~sep:"\n" |> sanitise
    in
    (match Core_unix.close_process (cmd_inc, cmd_outc) with
    | Ok _ ->
        [%test_result: string] cmd_output ~expect:expected;
        Stdio.print_endline "[OK]"
    | Error _ ->
        Stdio.print_endline "[ERROR]";
        encountered_err := true);
    Core_unix.remove output_asm_fname;
    Core_unix.remove output_bin_fname
  in
  Stdlib.Sys.readdir (test_dir ^ input_dir)
  |> Array.to_list
  |> List.filter ~f:(fun x -> String.(Stdlib.Filename.extension x = ".tig"))
  |> List.iter ~f:do_file;
  if !encountered_err then
    failwith "An error was encountered while running test programs!\n"
