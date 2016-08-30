open Ast
open Util
open Malfunction
open Malfunction_parser
open Malfunction_compiler

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
