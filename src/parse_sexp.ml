open Type
open Substs
open Sexplib
open Sexplib.Sexp

let rec ast_of_sexp = function
  | List (List [Atom "do"; e]::rest) ->
    `Sexec (expr_of_sexp e)::ast_of_sexp (List rest)
  | List (List [Atom "val"; Atom name; ty]::rest) ->
    `Sval (name, type_of_sexp ty)::ast_of_sexp (List rest)
  | List (List [Atom "let"; pattern; e]::rest) ->
    `Sbind (pattern_of_sexp pattern, expr_of_sexp e, fresh_tvar ())::ast_of_sexp (List rest)
  | List (List [Atom "type"; t; ty]::rest) ->
    `Stype (t, type_of_sexp ty)::ast_of_sexp (List rest)
  | List (List [Atom "dual"; a; b]::rest) ->
    `Sdual (type_of_sexp a, type_of_sexp b)::ast_of_sexp (List rest)
  | List (List [Atom "open"; Atom id]::rest) ->
    `Sopen (Id.Iident id)::ast_of_sexp (List rest)
  | _ -> assert false

and type_of_sexp = function
  | Atom a when a.[0] = Char.lowercase a.[0] -> Tvar a
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

and expr_of_sexp = function _ -> assert false

and pattern_of_sexp = function _ -> assert false

let parse_string str =
  let sexp = Sexp.of_string str in
  ast_of_sexp sexp
