type subst = EmptySubst | ExtendSubst of subst * Type.t * Type.t

let rec find_subst subst tvar =
  match subst with
  | EmptySubst -> None
  | ExtendSubst (_, tvar', ty)
    when tvar = tvar' -> Some ty
  | ExtendSubst (subst, _, _) -> find_subst subst tvar

let tvar_of_int n =
  let rec to_base26 d n =
    if n = 0 then d
    else to_base26 (n mod 26 :: d) (n / 26) in
  let a = Char.code 'a'
  and digits = to_base26 [] n in
  let num_digits = List.length digits in
  Type.Tvar (String.init num_digits (fun i -> List.nth digits i + a |> Char.chr))

let fresh_tvar =
  let next = ref 0 in
  fun () ->
    let res = tvar_of_int !next in
    next := !next + 1;
    res
