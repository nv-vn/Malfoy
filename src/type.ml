type t =
  (* a *)
  | Tvar of string
  (* A *)
  | Tconst of Id.ident
  (* A -> B *)
  | Tarrow of t * t
  (* (A, B) *)
  | Ttuple of t list
  (* A B *)
  | Tapply of t * t
  (* [> A | B c] *)
  | Tvariant of row
  (* -[> A | B c] *)
  | Ttag of t (* Needs dynamic check to ensure [t] resolves to [Tvariant] *)
  (* a* *)
  | Tdual of t

and row = {
  fields : (tag * t list) list;
  self   : t;
  more   : t;
  closed : bool
}

and tag =
  (* _ *)
  | Tunknown
  (* X *)
  | Tname of string

let (??) a = Tvar a
let (!!) a = Tconst a
let (@->) a b = Tarrow (a, b)

(* Copied from https://github.com/ocaml/ocaml/blob/trunk/typing/btype.ml#L184-L192 *)
let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let rec simplify_variant = function
  | {more = Tvariant more} as row ->
    {row with fields = row.fields @ more.fields}
  | {more = _} as row -> row

let rec string_of_type =
  let string_of_row {fields; closed} =
    let fields' =
      List.map (function Tunknown, ts ->
                           List.fold_left (fun t a -> Tapply (t, a)) (Tconst (Id.Iident "_")) ts |> string_of_type
                       | Tname s, ts ->
                           List.fold_left (fun t a -> Tapply (t, a)) (Tconst (Id.Iident s)) ts |> string_of_type)
               fields in
    "[" ^ (if closed then "< " else "> ")
        ^ String.concat " | " fields' ^ "]" in
  function
  | Tvar v -> v
  | Tconst c -> Id.string_of_ident c
  | Tarrow (t, u) -> string_of_type t ^ " -> " ^ string_of_type u
  | Ttuple ts -> "(" ^ (List.map string_of_type ts |> String.concat ", ") ^ ")"
  | Tapply (t, u) -> string_of_type t ^ " " ^ string_of_type u
  | Tvariant row -> string_of_row (simplify_variant row)
  | Ttag (Tvariant _ as var) -> "-" ^ string_of_type var
  | Ttag other -> "-(" ^ string_of_type other ^ ")"
  | Tdual t -> string_of_type t ^ "*"
