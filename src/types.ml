type t =
  (* a *)
  | Tvar of string
  (* A *)
  | Tconst of Ident.ident
  (* A as t *)
  | Tnamed of t * string
  (* A -> B *)
  | Tarrow of t * t
  (* (A, B) *)
  | Ttuple of t list
  (* A B *)
  | Tapply of t * t
  (* [> `A | `B c] *)
  | Tvariant of row
  (* -[> `A | `B c] *)
  | Ttag of t (* Needs dynamic check to ensure [t] resolves to [Tvariant] *)

and row = {
  fields : (tag * t list) list;
  closed : bool
}

and tag =
  (* `_ *)
  | Tunknown
  (* `X *)
  | Tname of string

let (??) a = Tvar a
let (!!) a = Tconst a
let (@->) a b = Tarrow (a, b)

let rec string_of_type =
  let string_of_row {fields; closed} =
    let fields' =
      List.map (function Tunknown, ts ->
                           string_of_type (Tapply (Tconst "`_", ts))
                       | Tname s, ts ->
                           string_of_type (Tapply (Tconst s, ts))) fields in
    "[" ^ if closed then "<" else ">"
        ^ String.concat " | " fields' ^ "]" in
  function
  | Tvar v -> v
  | Tconst c -> Ident.string_of_ident c
  | Tnamed (t, name) -> string_of_type t ^ " as " ^ name
  | Tarrow (t, u) -> string_of_type t ^ " -> " string_of_type u
  | Ttuple ts -> "(" ^ (List.map string_of_type ts |> String.concat ", ") ^ ")" 
  | Tapply (t, u) -> string_of_type t ^ " " ^ string_of_type u
  | Tvariant row -> string_of_row row
  | Ttag (Tvariant _ as var) -> "-" ^ string_of_type var
  | Ttag other -> "-(" ^ string_of_type other ^ ")"
