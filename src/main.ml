open Longident
open Malfunction
open Malfunction_parser
open Malfunction_compiler

let prelude = [
  "println", Mglobal (Ldot (Lident "Pervasives", "print_endline"))
]

let () =
  print_endline begin
    match Typecheck.check Ast.(Etuple []) Type.(Tvar "a") with
    | true -> Type.(string_of_type (Ttuple []))
    | false -> "Error checking types"
  end
