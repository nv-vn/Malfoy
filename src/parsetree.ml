type literal =
  (* 'a' *)
  | Lchar of char
  (* "ab" *)
  | Lstring of string
  (* 10 *)
  | Lint of int
  (* 1.5 *)
  | Lfloat of float

type variant_hash = int64

type pattern =
  (* _ *)
  | Pwildcard
  (* 1 *)
  | Pliteral of literal
  (* x *)
  | Pident of string
  (* x, y, z *)
  | Ptuple of pattern list
  (* Cons a Nil *)
  | Pconstruct of Ident.ident * pattern list
  (* `Hi a b c *)
  | Pvariant of variant_hash * pattern list

type expr =
  (* 'a' *)
  | Eliteral of literal
  (* abc *)
  | Eident of Ident.ident
  (* 1, 2, 3 *)
  | Etuple of expr list
  (* Cons 1 Nil *)
  | Econstruct of Ident.ident * expr list
  (* `Hi 1 2 3 *)
  | Evariant of variant_hash * expr list
  (* f 1 2 3 *)
  | Eapply of expr * expr
  (* \x -> x *)
  | Efun of pattern list * expr
  (* match x with [x] -> x | [] -> 0 *)
  | Ematch of expr * (pattern * expr) list
  (* let x = y in x *)
  | Ebind of string * expr * expr

type type_expr =
  (* () *)
  | Tident of Ident.ident
  (* a -> b *)
  | Tarrow of type_expr * type_expr
  (* TODO: Finish defining these *)

type stmt =
  (* do e end *)
  | Sexec of expr
  (* let x = y *)
  | Sbind of string * expr
  (* type t = s *)
  | Stype of string * type_expr
  (* open X *)
  | Sopen of Ident.ident
