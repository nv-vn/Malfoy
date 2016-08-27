open Util

type literal =
  (* 'a' *)
  | Lchar of char
  (* "ab" *)
  | Lstring of string
  (* 10 *)
  | Lint of int
  (* 1.5 *)
  | Lfloat of float

type pattern =
  (* _ *)
  | Pwildcard of Type.t
  (* 1 *)
  | Pliteral of literal * Type.t
  (* x *)
  | Pident of string * Type.t
  (* x, y, z *)
  | Ptuple of pattern list * Type.t
  (* Hi a b c *)
  | Pvariant of Type.tag * pattern list * Type.t

type expr =
  (* 'a' *)
  | Eliteral of literal * Type.t
  (* abc *)
  | Eident of Id.ident * Type.t
  (* 1, 2, 3 *)
  | Etuple of expr list * Type.t
  (* Hi 1 2 3 *)
  | Evariant of Type.tag * expr list * Type.t
  (* f 1 2 3 *)
  | Eapply of expr * expr * Type.t
  (* \x -> x *)
  | Efun of pattern * expr * Type.t
  (* match x with [x] -> x | [] -> 0 *)
  | Ematch of expr * (pattern * expr) list * Type.t
  (* let x = y in x *)
  | Ebind of pattern * expr * expr * Type.t

type stmt =
  (* do e end *)
  | Sexec of expr
  (* let x = y *)
  | Sbind of pattern * expr * Type.t
  (* type T = S *)
  | Stype of string * Type.t
  (* dual T <=> S *)
  | Sdual of Type.t * Type.t
  (* open X *)
  | Sopen of Id.ident

let get_expr_type = function
  | Eliteral (_, t) | Eident (_, t) | Etuple (_, t)
  | Evariant (_, _, t) | Eapply (_, _, t) | Efun (_, _, t)
  | Ematch (_, _, t) | Ebind (_, _, _, t)
    -> t

let string_of_literal = function
  | Lchar c -> String.make 1 c
  | Lstring s -> s
  | Lint i -> string_of_int i
  | Lfloat f -> string_of_float f

let rec string_of_pattern = function
  | Pwildcard _ -> "_"
  | Pliteral (l, _) -> string_of_literal l
  | Pident (id, _) -> id
  | Ptuple (ps, _) -> "(" ^ (List.map string_of_pattern ps |> String.concat ",") ^ ")"
  | Pvariant (Type.Tname tag, [], _) -> tag
  | Pvariant (Type.Tname tag, ps, _) -> tag ^ "(" ^ (List.map string_of_pattern ps |> String.concat ",") ^ ")"
  | _ -> "Invalid pattern"

let string_of_binds ast =
  let rec expr = function
    | Etuple (es, _) -> es >>= expr
    | Evariant (_, args, _) -> args >>= expr
    | Eapply (f, x, _) -> expr f @ expr x
    | Efun (_, x, _) -> expr x
    | Ematch (_, branches, _) -> branches >>= fun (_, e) -> expr e
    | Ebind (pat, e, ctx, t) ->
      let decl = string_of_pattern pat ^ " : " ^ Type.string_of_type (get_expr_type e) in
      decl::expr e@expr ctx
    | _ -> [] in
  let stmt = function
    | Sexec e -> expr e
    | Sbind (pat, e, _) ->
      let decl = string_of_pattern pat ^ " : " ^ Type.string_of_type (get_expr_type e) in
      decl::expr e
    | _ -> [] in
  stmt ast |> String.concat "\n"
