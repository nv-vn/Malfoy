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
  | Sexec of expr list
  (* let x = y *)
  | Sbind of pattern * expr * Type.t
  (* type T = S *)
  | Stype of string * Type.t
  (* dual T <=> S *)
  | Sdual of Type.t * Type.t
  (* open X *)
  | Sopen of Id.ident
