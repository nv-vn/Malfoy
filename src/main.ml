let interpret () =
  (* Should probably get a readline solution later...
     Start using Core? Use LambdaTerm? *)
  while true do
    print_string "Malfoy> ";
    flush stdout;
    let text = "(" ^ input_line stdin ^ ")" in
    let ast = Desugar.desugar @@ Parse_sexp.parse_string text in
    let annotated = Inference.type_ast ~env:Type_env.(env_of_list default) ast in
    List.iter (fun stmt -> Ast.string_of_binds stmt |> print_endline) annotated
  done

let compile () =
  let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    String.trim s in
  for i = 2 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i) in
    print_endline (load_file file);
    let ast = Desugar.desugar @@ Parse_sexp.parse_string (load_file file) in
    let annotated = Inference.type_ast ~env:Type_env.(env_of_list default) ast in
    List.iter (fun stmt -> Ast.string_of_binds stmt |> print_endline) annotated
  done

let () =
  let usage = "malfoy [-c <files> | -i]" in
  Arg.parse [
    "-i", Arg.Unit interpret, "Launch a Malfoy REPL";
    "-c", Arg.Unit compile, "Compile Malfoy files"
  ] (fun _ -> ()) usage
