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
  | Eident i, ty -> scope#lookup i = ty
  | Etuple es, Ttuple ts -> List.map2 check es ts |> List.fold_left (&&) true
  | Econstruct (i, args), ty ->
    begin
      let i_ty = scope#lookup i in
      let rec loop args ty =
        match args, ty with
        | arg::args, Tarrow (a, b) when check arg a -> loop args b
        | arg::args, Tarrow (a, b) -> false (* Type-checking when clause failed *)
        | arg::args, _ -> false (* No more room to apply arguments *)
        | [], ty -> true in
      loop args i_ty
    end
  (* | Evariant (hash, args), ty -> ... *)
  | _ -> true
