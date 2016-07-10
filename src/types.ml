type t =
  (* a *)
  | Tvar of string
  (* A as t *)
  | Tnamed of t * string
  (* A -> B *)
  | Tarrow of t * t
  (* (A, B) *)
  | Ttuple of t list
  (* A B *)
  | Tconstr of t * t
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
  | Lunknown
  (* `X *)
  | Lname of string
