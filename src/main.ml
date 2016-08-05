open Longident
open Malfunction
open Malfunction_parser
open Malfunction_compiler

let prelude = [
  "println", Mglobal (Ldot (Lident "Pervasives", "print_endline"))
]

let () =
  let _ =
    let open Type in
    Inference.unify_rows [Tname "A", [Tvar   "a"]]
                         [Tname "A", [Tconst (Id.Iident "Int")]]
                         Substs.EmptySubst
                         Ast.(Eliteral (Lchar 'a', None)) in
  print_endline begin
    Type.(string_of_type (Ttuple []))
  end
