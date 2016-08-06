open Longident
open Malfunction
open Malfunction_parser
open Malfunction_compiler

let prelude = [
  "println", Mglobal (Ldot (Lident "Pervasives", "print_endline"))
]

let () =
  let subst =
    let open Type in
    Inference.unify_rows ([Tname "A", [Tvar   "a"];
                           Tname "B", [Tconst (Id.Iident "Int")]], Tvar "more1")
                         ([Tname "A", [Tconst (Id.Iident "Int")]], Tvar "more2")
                         Substs.EmptySubst
                         Ast.(Eliteral (Lchar 'a', None)) in
  print_endline (Substs.string_of_subst subst);
  print_endline begin
    Type.(string_of_type (Ttuple []))
  end
