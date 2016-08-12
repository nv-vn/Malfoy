open Id
open Ast
open Type
open Util
open Substs
open Type_env

let char = !!(Iident "Char")
and string = !!(Iident "String")
and int = !!(Iident "Int")
and float = !!(Iident "Float")

let rec apply_substitutions ty substs =
  let subst ty = apply_substitutions ty substs in
  match ty with
  | Tvar a as tvar -> begin
      match find_subst substs tvar with
      | Some value -> value
      | None -> Tvar a
    end
  | Tarrow (a, b) -> Tarrow (subst a, subst b)
  | Ttuple ts -> Ttuple (List.map subst ts)
  | Tapply (a, b) -> Tapply (subst a, subst b)
  | Tvariant row ->
    Tvariant {row with
              fields =
                List.map (fun (tag, ts) ->
                           (tag, List.map subst ts))
                  row.fields;
              more = subst row.more}
  | Ttag t -> subst t
  | ty -> ty

let rec doesn't_occur tvar ty =
  let all f = List.fold_left (fun b x -> (f x) && b) true in
  match ty with
  | Tvar a as tvar' -> tvar <> tvar'
  | Tarrow (a, b) -> doesn't_occur tvar a && doesn't_occur tvar b
  | Ttuple ts -> all (doesn't_occur tvar) ts
  | Tapply (a, b) -> doesn't_occur tvar a && doesn't_occur tvar b
  | Tvariant row -> all (fun (_, ts) -> all (doesn't_occur tvar) ts) row.fields
  | Ttag t -> doesn't_occur tvar t
  | _ -> true

let rec unify_rows (fields1, more1) (fields2, more2) subst =
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
    if List.length args1 <> List.length args2 then assert false; (* Make sure they both have the same number of args *)
    let both = List.map2 (fun a b -> (a, b)) args1 args2 in
    (* For every pair, unify them and add them to the list of substitutions *)
    List.map
      (fun (arg1, arg2) ->
         (* Remove the redundant substitutions after unification *)
         unify arg1 arg2 subst -@ subst)
      both in
  let absent gs xs =
    List.filter (fun (tag, _) -> not @@ List.exists ((=) tag) gs) xs in
  let fields1' = absent present_in_both fields1
  and fields2' = absent present_in_both fields2
  and more' = fresh_tvar () in
  let left = Tvariant { fields = fields1'; self = fresh_tvar ();
                        more = more'; closed = true }
  and right = Tvariant { fields = fields2'; self = fresh_tvar ();
                         more = more'; closed = true } in
  (* Make sure to unify each type with the opposite! *)
  let left_subst = unify left more2 subst -@ subst
  and right_subst = unify right more1 subst -@ subst in
  let all = List.concat [common_subs; [left_subst]; [right_subst]] in
  (* Add all the substitutions together to the substitutions being unified *)
  List.fold_left (+@) subst all

and unify t1 t2 subst =
  let t1 = apply_substitutions t1 subst
  and t2 = apply_substitutions t2 subst in
  match t1, t2 with
  | _, _ when t1 = t2 -> subst
  | Tvar _, _ when doesn't_occur t1 t2 ->
    ExtendSubst (subst, t1, t2)
  | _, Tvar _ when doesn't_occur t2 t1 ->
    ExtendSubst (subst, t2, t1)
  | Tarrow (a1, b1), Tarrow (a2, b2) | Tapply (a1, b1), Tapply (a2, b2) ->
    let subst = unify a1 a2 subst in
    let subst = unify b1 b2 subst in
    subst
  | Ttuple t1s, Ttuple t2s ->
    let ts = List.map2 (fun t1 t2 -> (t1, t2)) t1s t2s in
    List.fold_left (fun subst (t1, t2) -> unify t1 t2 subst) subst ts
  | Tvariant row1, Tvariant row2 ->
    unify_rows (row1.fields, row1.more) (row2.fields, row2.more) subst
  | Ttag t1, Ttag t2 -> unify t1 t2 subst
  | _, _ ->
    print_endline ("Couldn't unify types: \n" ^ string_of_type t1 ^ " and \n " ^ string_of_type t2);
    assert false (* Can't unify types *)

let type_of_literal lit =
  match lit with
  | Lchar _ -> char
  | Lstring _ -> string
  | Lint _ -> int
  | Lfloat _ -> float

let get_expr_type = function
  | Eliteral (_, t) | Eident (_, t) | Etuple (_, t)
  | Evariant (_, _, t) | Eapply (_, _, t) | Efun (_, _, t)
  | Ematch (_, _, t) | Ebind (_, _, _, t)
    -> t

let rec type_of_pattern = function
  | Pwildcard t -> t
  | Pliteral (l, _) -> type_of_literal l
  | Pident (_, t) -> t
  | Ptuple (ts, _) -> Ttuple (List.map type_of_pattern ts)
  | Pvariant (tag, args, _) ->
    let arg_ts = List.map type_of_pattern args in
    Tvariant { fields = [tag, arg_ts]
             ; self = fresh_tvar ()
             ; more = fresh_tvar ()
             ; closed = false
             }
  | _ -> fresh_tvar ()

let rec collect_substs subst env = function
  | Eliteral (l, t) ->
    unify t (type_of_literal l) subst
  | Eident (id, t) ->
    unify t (unsafe_lookup env id) subst
  | Etuple (es, t) ->
    let ts = List.map get_expr_type es in
    unify t (Ttuple ts) subst
  | _ -> subst
