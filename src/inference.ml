open Id
open Ast
open Type
open Substs
open Type_env

let scope = new env default

let char = !!(Iident "Char")
and string = !!(Iident "String")
and int = !!(Iident "Int")
and float = !!(Iident "Float")

let type_of_literal lit =
  match lit with
  | Lchar _ -> char
  | Lstring _ -> string
  | Lint _ -> int
  | Lfloat _ -> float

let rec apply_substitutions ty substs =
  let subst ty = apply_substitutions ty substs in
  match ty with
  | Tvar a as tvar -> begin
      match find_subst substs tvar with
      | Some value -> value
      | None -> Tvar a
    end
  | Tnamed (a, var') -> Tnamed (subst a, var')
  | Tarrow (a, b) -> Tarrow (subst a, subst b)
  | Ttuple ts -> Ttuple (List.map subst ts)
  | Tapply (a, b) -> Tapply (subst a, subst b)
  | Tvariant row ->
    Tvariant {row with
              fields =
                List.map (fun (tag, ts) ->
                           (tag, List.map subst ts))
                  row.fields}
  | Ttag t -> subst t
  | ty -> ty

let rec doesn't_occur tvar ty =
  let any f xs = List.filter f xs |> List.length > 0 in
  match ty with
  | Tvar a as tvar' when tvar = tvar' -> true
  | Tnamed (t, _) -> doesn't_occur tvar t
  | Tarrow (a, b) -> doesn't_occur tvar a || doesn't_occur tvar b
  | Ttuple ts -> any (doesn't_occur tvar) ts
  | Tapply (a, b) -> doesn't_occur tvar a || doesn't_occur tvar b
  | Tvariant row -> any (fun (_, ts) -> any (doesn't_occur tvar) ts) row.fields
  | Ttag t -> doesn't_occur tvar t
  | _ -> true

let rec unify_rows fields1 fields2 subst expr =
  let (>>=) xs f = List.map f xs |> List.flatten in
  (* Get all sets of matching tags *)
  let intersect xs ys =
    let xtags = List.map fst xs
    and ytags = List.map fst ys in
    xtags >>= (fun xtag -> List.filter ((=) xtag) ytags) in
  (* Isolate the tags that are present in both sets *)
  let present_in_both = intersect fields1 fields2 in
  let common_subs =
    (* For every tag present in both, *)
    present_in_both >>= fun tag ->
    (* Collect the args for that tag in both sets of fields and group them in pairs *)
    let args1 = List.assoc tag fields1 and args2 = List.assoc tag fields2 in
    let both = List.map2 (fun a b -> (a, b)) args1 args2 in
    (* For every pair, unify them and add them to the list of substitutions *)
    List.map
      (fun (arg1, arg2) ->
         (* Remove the redundant substitutions after unification *)
         unify arg1 arg2 subst expr -@ subst)
      both in
  (* Add all the substitutions together to the substitutions being unified *)
  List.fold_left (+@) subst common_subs
(*
  let absent gs xs =
    List.filter (fun (tag, _) -> not @@ List.exists ((=) tag) gs) xs in
  let left_subs = absent present_in_both fields1 |> List.map fst
  and right_subs = absent present_in_both fields2 |> List.map fst in
  let all = List.concat [common_subs; left_subs; right_subs] in
  List.fold_left
    (fun (arg1, arg2) subst ->
      unify arg1 arg2 subst expr) subst all *)

and unify t1 t2 subst expr =
  let t1 = apply_substitutions t1 subst
  and t2 = apply_substitutions t2 subst in
  match t1, t2 with
  | _, _ when t1 = t2 -> subst
  | Tvar _, _ when doesn't_occur t1 t2 ->
    ExtendSubst (subst, t1, t2)
  | _, Tvar _ when doesn't_occur t2 t1 ->
    ExtendSubst (subst, t2, t1)
  | Tarrow (a1, b1), Tarrow (a2, b2) | Tapply (a1, b1), Tapply (a2, b2) ->
    let subst = unify a1 a2 subst expr in
    let subst = unify b1 b2 subst expr in
    subst
  | Ttuple t1s, Ttuple t2s ->
    let ts = List.map2 (fun t1 t2 -> (t1, t2)) t1s t2s in
    List.fold_left (fun subst (t1, t2) -> unify t1 t2 subst expr) subst ts
  (* (* How can I unify a type with row polymorphism but still intersect the types? *)
  | Tvariant row1, Tvariant row2 ->
    let fields = List.map2 (fun field1 field2 -> (field1, field2)) row1.fields row2.fields in
    let fields = List.map (fun ((tag1, t1s), (tag2, t2s)) ->
                             if tag1 = tag2 then
                               let args = List.map2 (fun arg1 arg2 -> (arg1, arg2)) t1s t2s in
                               List.fold_left (fun subst (t1, t2) -> unify t1 t2 subst expr) subst args
                             else subst) fields in
    subst *)
  | Ttag t1, Ttag t2 -> unify t1 t2 subst expr
  | _, _ -> assert false (* Can't unify types *)
