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
    Inference.unify t1 t2 Substs.EmptySubst in
  print_endline ("Unifying types: " ^ string_of_type t1 ^ " and " ^ string_of_type t2);
  print_endline ("Found substitutions:\n" ^ Substs.string_of_subst subst);
  print_endline ("Got type: " ^ string_of_type (Inference.apply_substitutions t1 subst));
  let example_ast =
    let open Ast in
    let open Substs in
    (* let f = \x -> x in f 0 *)
    Ebind (Pident ("f", fresh_tvar ()),
           Efun (Pident ("x", fresh_tvar ()),
                 Eident (Id.Iident "x", fresh_tvar ()),
                 fresh_tvar ()),
           Eapply (Eident (Id.Iident "f", fresh_tvar ()),
                   Eliteral (Lint 0, fresh_tvar ()),
                   fresh_tvar ()),
           fresh_tvar ()) in
  match Inference.infer_types example_ast with
  | Ebind (_, Efun (_, _, t), _, _) -> print_endline (string_of_type t)
