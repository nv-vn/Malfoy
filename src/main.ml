open Longident
open Malfunction
open Malfunction_parser
open Malfunction_compiler

let prelude = [
  "println", Mglobal (Ldot (Lident "Pervasives", "print_endline"))
]

let () =
  let file = Sys.argv.(1) in
  let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = String.create n in
    really_input ic s 0 n;
    close_in ic;
    String.trim s in
  print_endline (load_file file);
  let ast = Desugar.desugar @@ Parse_sexp.parse_string (load_file file) in
  let annotated = Inference.type_ast ast in
  ()
