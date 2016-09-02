open Ast
open Util
open Malfunction
open Malfunction_parser
open Malfunction_compiler

let literal_of_ast = function
  | Lchar c -> `Lchar c
  | Lstring s -> `Lstring s
  | Lint i -> `Lint i
  | Lfloat f -> `Lfloat f

let rec pattern_of_ast = function
  | Pwildcard _ -> `Pwildcard
  | Pliteral (l, _) -> `Pliteral (literal_of_ast l)
  | Pident (i, _) -> `Pident i
  | Ptuple (ps, _) -> `Ptuple (List.map pattern_of_ast ps)
  | Pvariant (tag, args, _) -> `Pvariant (tag, List.map pattern_of_ast args)

let rec expr_of_ast = function
  | Eliteral (l, _) -> `Eliteral (literal_of_ast l)
  | Eident (id, _) -> `Eident id
  | Etuple (es, _) -> `Etuple (List.map expr_of_ast es)
  | Evariant (tag, args, _) -> `Evariant (tag, List.map expr_of_ast args)
  | Eapply (f, x, _) -> `Eapply (expr_of_ast f, expr_of_ast x)
  | Efun (pat, e, _) -> `Efun (pattern_of_ast pat, expr_of_ast e)
  | Ematch (e, branches, _) -> `Ematch (expr_of_ast e, List.map (fun (p, e) -> (pattern_of_ast p, expr_of_ast e)) branches)
  | Ebind (pat, e, ctx, _) -> `Ebind (pattern_of_ast pat, expr_of_ast e, expr_of_ast ctx)

let ast_of_stmt = function
  | Sexec e -> `Sexec (expr_of_ast e)
  | Sbind (pat, e) -> `Sbind (pattern_of_ast pat, expr_of_ast e)
  | Sopen m -> `Sopen m
  | _ -> `Sblank

let rec map_expr f = function
  | `Etuple es -> `Etuple (List.map f es)
  | `Evariant (tag, args) -> `Evariant (tag, List.map f args)
  | `Eapply (f', x) -> `Eapply (f f', f x)
  | `Efun (pat, e) -> `Efun (pat, f e)
  | `Ematch (x, branches) -> `Ematch (f x, List.map (fun (p, e) -> (p, f e)) branches)
  | `Ebind (pat, e, ctx) -> rebind pat (f e) (f ctx) f
  | `Eget (e, n) -> `Eget (f e, n)
  | expr -> expr

and rebind pat e ctx f = match pat with
  (* TODO: Recursively expand patterns? *)
  | `Ptuple ps -> begin
      let rec fold var n = function
        | [] -> map_expr f ctx
        | p::ps -> `Ebind (p, `Eget (var, n), fold var (n + 1) ps) in
      let id = Id.fresh_var "%%tuple" in
      `Ebind (`Pident id, map_expr f e, fold (`Eident id) 0 ps)
    end
  | `Pvariant (tag, ps) -> begin
      let rec fold var n = function
        | [] -> map_expr f ctx
        | p::ps -> `Ebind (p, `Eget (var, n), fold var (n + 1) ps) in
      let id = Id.fresh_var "%%tuple" in
      (* TODO: Add tag check! *)
      `Ebind (`Pident id, map_expr f e, fold (`Eident id) 0 ps)
    end
  | _ -> `Ebind (pat, map_expr f e, map_expr f ctx)

module EnvMap = Map.Make (struct
    type t = Id.ident
    let compare = Pervasives.compare
  end)

let rec expand_bindings env pat e = match pat, e with
  | Pwildcard _, e -> [`Unnamed (expand_expression e)], env
  | Pident (id, _), e ->
    let var = Ident.create id in
    [`Recursive [id, expand_expression e]], EnvMap.add (Id.Iident id) var env
  (* Tuples and variants are expanded to code that deconstructs the value and binds the
     extracted data.

     Tuples are layed out as (block (tag 0) v1 v2 ... vN). Extract with (field N block).
     Can also be stored as vectors: (makevec LENGTH VAL) => repeats VAL per slot.
     (load VEC N) => VEC.(N) and (store VEC N VAL) => VEC.(N) <- VAL.

     <VEC-OP>.byte lets you deal with array of single bytes...

     Variants are layed out as (block (tag 1) tag v1 v2 ... vN). Extract with (field (N+1) block). *)
  | Pliteral _, _ -> assert false (* Matching against literals when binding makes little sense *)

and expand_expression env = function _ -> Mint (`Int 0)
