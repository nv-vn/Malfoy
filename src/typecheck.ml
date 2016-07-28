open Env
open Ident
open Types

let scope = new env default
and type_vars = new env [| |]

let char = !!(Iident "Char")
and string = !!(Iident "String")
and int = !!(Iident "Int")
and float = !!(Iident "Float")

let check_literal lit ty =
  (* TODO: Check for types synonyms, etc. *)
  match lit, ty with
  | Lchar _, char -> true
  | Lstring _, string -> true
  | Lint _, int -> true
  | Lfloat _, float -> true
  | _, _ -> false

let rec check ast ty =
  (* TODO: Perform subtyping checks *)
  match ast, ty with
  | Eliteral e, ty -> check_literal e ty
  | _ -> true
