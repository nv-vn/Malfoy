open Ast
open Type
open Substs

let ast_of_literal = function
  | `Lchar c -> Lchar c
  | `Lstring s -> Lstring s
  | `Lint i -> Lint i
  | `Lfloat f -> Lfloat f

let rec ast_of_pattern = function
  | `Pwildcard t -> Pwildcard t
  | `Pliteral (l, t) -> Pliteral (ast_of_literal l, t)
  | `Pident (i, t) -> Pident (i, t)
  | `Ptuple (ps, t) -> Ptuple (List.map ast_of_pattern ps, t)
  | `Pvariant (tag, args, t) -> Pvariant (tag, List.map ast_of_pattern args, t)
  | _ -> assert false (* Remeber, we're feeding in types from nanopasses *)

let rec ast_of_expr = function
  | `Eliteral (l, t) -> Eliteral (l, t)
  | `Eident (id, t) -> Eident (id, t)
  | `Etuple (es, t) -> Etuple (List.map ast_of_expr es, t)
  | `Evariant (tag, args, t) -> Evariant (tag, List.map ast_of_expr args, t)
  | `Eapply (f, x, t) -> Eapply (ast_of_expr f, ast_of_expr x, t)
  | `Efun (pat, e, t) -> Efun (ast_of_pattern pat, ast_of_expr e, t)
  | `Ematch (e, branches, t) -> Ematch (ast_of_expr e, List.map (fun (p, e) -> (ast_of_pattern p, ast_of_expr e)) branches, t)
  | `Ebind (pat, e, ctx, t) -> Ebind (ast_of_pattern pat, ast_of_expr e, ast_of_expr ctx, t)
  | _ -> assert false (* Remeber, we're feeding in types from nanopasses *)

let ast_of_stmt = function
  | `Sexec e -> Sexec (List.map ast_of_expr e)
  | `Sbind (pat, e, t) -> Sbind (ast_of_pattern pat, ast_of_expr e, t)
  | `Stype (s, t) -> Stype (s, t)
  | `Sdual (t1, t2) -> Sdual (t1, t2)
  | `Sopen m -> Sopen m
  | _ -> assert false (* Remeber, we're feeding in types from nanopasses *)

let rec map_expr f = function
  | `Etuple (es, t) -> `Etuple (List.map f es, t)
  | `Evariant (tag, args, t) -> `Evariant (tag, List.map f args, t)
  | `Eapply (f', x, t) -> `Eapply (f f', f x, t)
  | `Efun (pat, e, t) -> `Efun (pat, f e, t)
  | `Ematch (x, branches, t) -> `Ematch (f x, List.map (fun (p, e) -> (p, f e)) branches, t)
  | `Ebind (pat, e, ctx, t) -> `Ebind (pat, f e, f ctx, t)
  | `Eif (x, i, e, t) -> `Eif (f x, f i, f e, t)
  | `Ebegin (es, t) -> `Ebegin (List.map f es, t)
  | expr -> expr

let rec expand_if = function
  | `Eif (x, i, e, t) ->
    `Ematch (x, [ `Evariant ("True",  [], fresh_tvar ()), i
                ; `Evariant ("False", [], fresh_tvar ()), e], t)
  | expr -> map_expr expand_if expr

let rec expand_begin = function
  | `Ebegin ([], t) -> assert false (* Can't expand empty expressions *)
  | `Ebegin ([e], t) -> e
  | `Ebegin (e::es, t) ->
    let rest = `Ebegin (es, t) in
    `Ebind (`Pwildcard (fresh_tvar ()), map_expr expand_begin e, expand_begin rest, t)
  | expr -> map_expr expand_begin expr

let desugar statements =
  let expr_passes e = expand_begin (expand_if e) in
  let statements' =
    List.map (function `Sexec e ->
                       `Sexec (List.map expr_passes e)
                     | `Sbind (pat, e, t) ->
                       `Sbind (pat, expr_passes e, t)
                     | stmt -> stmt) statements in
  List.map ast_of_stmt statements'
