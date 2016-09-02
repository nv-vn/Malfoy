type ident =
  (* x *)
  | Iident of string
  (* M.<x> *)
  | Iqualified of string * ident
  (* x y *)
  | Iapply of ident * ident

let rec string_of_ident = function
  | Iident s -> s
  | Iqualified (m, i) -> m ^ "." ^ string_of_ident i
  | Iapply (i, j) -> string_of_ident i ^ " " ^ string_of_ident j

let rec base_ident = function
  | Iident s -> s
  | Iqualified (_, i) -> base_ident i
  | Iapply (i, _) -> base_ident i

let fresh_var =
  let next = ref 0 in
  fun base ->
    let res = base ^ (string_of_int !next) in
    next := !next + 1;
    Iident res
