open Longident
open Malfunction
open Malfunction_parser
open Malfunction_compiler

let prelude = [
  "println", Mglobal (Ldot (Lident "Pervasives", "print_endline"))
]

let () =
  let open Type in
  (* [< A a | B Int] + more1 as t1 *)
  let t1 = Tvariant { fields = [ Tname "A", [ Tvar "a" ]
                               ; Tname "B", [ Tconst (Id.Iident "Int") ]
                               ]
                    ; more = Tvar "more1"
                    ; self = Tvar "t1"
                    ; closed = true
                    }
  (* [< B Int] + more2 as t2 *)
  and t2 = Tvariant { fields = [ Tname "A", [ Tconst (Id.Iident "Int") ] ]
                    ; more = Tvar "more2"
                    ; self = Tvar "t2"
                    ; closed = true
                    } in
  let subst =
    let open Type in
    Inference.unify t1 t2 Substs.EmptySubst Ast.(Eliteral (Lchar 'a', None)) in
  print_endline ("Unifying types: " ^ string_of_type t1 ^ " and " ^ string_of_type t2);
  print_endline ("Found substitutions:\n" ^ Substs.string_of_subst subst);
  print_endline ("Got type: " ^ string_of_type (Inference.apply_substitutions t1 subst))
