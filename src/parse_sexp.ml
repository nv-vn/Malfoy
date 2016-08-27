open Type
open Substs
open Sexplib
open Sexplib.Sexp

let is_uident a =
  match a.[0] with
  | 'A'..'Z' -> true
  | _ -> false

let is_lident a = not (is_uident a)

let rec ast_of_sexp =
  let check_rest = function
    | [] -> []
    | rest -> ast_of_sexp (List rest) in
  function
  | List (List [Atom "do"; e]::rest) ->
    `Sexec (expr_of_sexp e)::check_rest rest
  | List (List [Atom "val"; Atom name; ty]::rest) ->
    `Sval (name, type_of_sexp ty)::check_rest rest
  | List (List [Atom "let"; pattern; e]::rest) ->
    `Sbind (pattern_of_sexp pattern, expr_of_sexp e)::check_rest rest
  | List (List [Atom "type"; Atom t; ty]::rest) ->
    `Stype (t, type_of_sexp ty)::check_rest rest
  | List (List [Atom "dual"; a; b]::rest) ->
    `Sdual (type_of_sexp a, type_of_sexp b)::check_rest rest
  | List (List [Atom "open"; Atom id]::rest) ->
    `Sopen (Id.Iident id)::check_rest rest
  | _ -> assert false

and type_of_sexp = function
  | Atom a when is_lident a -> Tvar a
  | Atom a -> Tconst (Id.Iident a)
  | List [Atom "->"; a; b] -> Tarrow (type_of_sexp a, type_of_sexp b)
  | List (Atom ","::rest) -> Ttuple (List.map type_of_sexp rest)
  | List (Atom ">"::rest) -> Tvariant { self = fresh_tvar ()
                                      ; more = fresh_tvar ()
                                      ; closed = false
                                      ; fields = List.map variant_of_sexp rest
                                      }
  | List (Atom "<"::rest) -> Tvariant { self = fresh_tvar ()
                                      ; more = fresh_tvar ()
                                      ; closed = true
                                      ; fields = List.map variant_of_sexp rest
                                      }
  | List [Atom "-"; ty] -> Ttag (type_of_sexp ty)
  | List [Atom "*"; ty] -> Tdual (type_of_sexp ty)
  | List (t::rest) -> List.fold_left (fun ty ty' -> Tapply (ty, type_of_sexp ty')) (type_of_sexp t) rest
  | List [] -> Tconst (Id.Iident "0")

and variant_of_sexp = function
  | Atom "_" -> (Tunknown, [])
  | Atom a -> (Tname a, [])
  | List (Atom "_"::rest) -> (Tunknown, List.map type_of_sexp rest)
  | List (Atom a::rest) -> (Tname a, List.map type_of_sexp rest)
  | _ -> assert false

and expr_of_sexp = function
  | List [Atom "<INT>"; Atom x] -> `Eliteral (`Lint (int_of_string x), fresh_tvar ())
  | List [Atom "<FLOAT>"; Atom x] -> `Eliteral (`Lfloat (float_of_string x), fresh_tvar ())
  | List [Atom "<STRING>"; Atom x] ->
    let last = String.length x - 1 in
    let s = String.sub x 1 (last - 1) in
    `Eliteral (`Lstring s, fresh_tvar ())
  | List (Atom ","::xs) -> `Etuple (List.map expr_of_sexp xs, fresh_tvar ())
  | Atom a when is_lident a-> `Eident (Id.Iident a, fresh_tvar ())
  | Atom a -> `Evariant (Tname a, [], fresh_tvar ())
  | List [Atom "fn"; List args; e] ->
    List.fold_right (fun arg inner -> `Efun (pattern_of_sexp arg, inner, fresh_tvar ())) args (expr_of_sexp e)
  | List (Atom "match"::x::branches) ->
    `Ematch (expr_of_sexp x, List.map (fun (List [p; e]) -> (pattern_of_sexp p, expr_of_sexp e)) branches, fresh_tvar ())
  | List [Atom "let"; pat; e; ctx] ->
    `Ebind (pattern_of_sexp pat, expr_of_sexp e, expr_of_sexp ctx, fresh_tvar ())
  | List (Atom tag::args) when is_uident tag ->
    `Evariant (Tname tag, List.map expr_of_sexp args, fresh_tvar ())
  | List (f::xs) -> List.fold_left (fun f x -> `Eapply (f, expr_of_sexp x, fresh_tvar ())) (expr_of_sexp f) xs
  | _ -> assert false

and pattern_of_sexp = function
  | Atom "_" -> `Pwildcard (fresh_tvar ())
  | List [Atom "<INT>"; Atom x] -> `Pliteral (`Lint (int_of_string x), fresh_tvar ())
  | List [Atom "<FLOAT>"; Atom x] -> `Pliteral (`Lfloat (float_of_string x), fresh_tvar ())
  | List [Atom "<STRING>"; Atom x] ->
    let last = String.length x - 1 in
    let s = String.sub x 1 (last - 1) in
    `Pliteral (`Lstring s, fresh_tvar ())
  | List (Atom ","::xs) -> `Ptuple (List.map pattern_of_sexp xs, fresh_tvar ())
  | Atom a when is_lident a -> `Pident (a, fresh_tvar ())
  | Atom a -> `Pvariant (Tname a, [], fresh_tvar ())
  | List (Atom tag::args) -> `Pvariant (Tname tag, List.map pattern_of_sexp args, fresh_tvar ())
  | _ -> assert false

let parse_string str =
  let sexp = Sexp.of_string str in
  ast_of_sexp sexp
