type literal =
  (* 'a' *)
  | Lchar of char
  (* "ab" *)
  | Lstring of string
  (* 10 *)
  | Lint of int
  (* 1.5 *)
  | Lfloat of float

type variant_hash = int

type pattern =
  (* _ *)
  | Pwildcard of Type.t option
  (* 1 *)
  | Pliteral of literal * Type.t option
  (* x *)
  | Pident of string * Type.t option
  (* x, y, z *)
  | Ptuple of pattern list * Type.t option
  (* Hi a b c *)
  | Pvariant of variant_hash * pattern list * Type.t option

type expr =
  (* 'a' *)
  | Eliteral of literal * Type.t option
  (* abc *)
  | Eident of Id.ident * Type.t option
  (* 1, 2, 3 *)
  | Etuple of expr list * Type.t option
  (* Hi 1 2 3 *)
  | Evariant of variant_hash * expr list * Type.t option
  (* f 1 2 3 *)
  | Eapply of expr * expr * Type.t option
  (* \x -> x *)
  | Efun of pattern list * expr * Type.t option
  (* match x with [x] -> x | [] -> 0 *)
  | Ematch of expr * (pattern * expr) list * Type.t option
  (* let x = y in x *)
  | Ebind of string * expr * expr * Type.t option

type stmt =
  (* do e end *)
  | Sexec of expr
  (* let x = y *)
  | Sbind of string * expr * Type.t option
  (* type T = S *)
  | Stype of string * Type.t
  (* dual T <=> S *)
  | Sdual of Type.t * Type.t
  (* open X *)
  | Sopen of Id.ident
