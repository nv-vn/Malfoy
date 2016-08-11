open Id
open Ast
open Type
open Util
open Substs
open Type_env

let scope = new env default

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

let rec unify_rows (fields1, more1) (fields2, more2) subst expr =
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
         unify arg1 arg2 subst expr -@ subst)
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
  let left_subst = unify left more2 subst expr -@ subst
  and right_subst = unify right more1 subst expr -@ subst in
  let all = List.concat [common_subs; [left_subst]; [right_subst]] in
  (* Add all the substitutions together to the substitutions being unified *)
  List.fold_left (+@) subst all

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
  | Tvariant row1, Tvariant row2 ->
    unify_rows (row1.fields, row1.more) (row2.fields, row2.more) subst expr
  | Ttag t1, Ttag t2 -> unify t1 t2 subst expr
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
  | Eliteral (_, Some t) | Eident (_, Some t) | Etuple (_, Some t)
  | Evariant (_, _, Some t) | Eapply (_, _, Some t) | Efun (_, _, Some t)
  | Ematch (_, _, Some t) | Ebind (_, _, _, Some t)
    -> t
  | _ -> assert false

let rec type_map subst scope exprs =
  let rec loop ts = function
    | [], subst -> ts, subst
    | x::xs, subst ->
      let t, subst' = type_of_expr subst scope x in
      loop ([t]@ts) (xs, subst') in
  loop [] (exprs, subst)

and type_of_pattern = function
  | Pwildcard (Some t) -> t
  | Pwildcard None -> fresh_tvar ()
  | Pliteral (l, _) -> type_of_literal l
  | Pident (_, Some t) -> t
  | Pident (_, None) -> fresh_tvar ()
  | Ptuple (ts, _) -> Ttuple (List.map type_of_pattern ts)
  | Pvariant (tag, args, _) ->
    let arg_ts = List.map type_of_pattern args in
    Tvariant { fields = [tag, arg_ts]
             ; self = fresh_tvar ()
             ; more = fresh_tvar ()
             ; closed = false
             }
  | _ -> fresh_tvar ()

and bind_pattern scope = function
  | Pident (x, Some t) ->
    scope#register x t
  | Pident (x, None) ->
    scope#register x (fresh_tvar ())
  | Ptuple (ts, _) ->
    List.iter (bind_pattern scope) ts
  | Pvariant (_, args, _) ->
    List.iter (bind_pattern scope) args
  | _ -> ()

and type_of_expr subst scope = function
  | Eliteral (l, _) -> type_of_literal l, subst
  | Eident (id, _) -> unsafe_get (scope#lookup id), subst
  | Etuple (xs, _) ->
    let ts, subst = type_map subst scope xs in
    Ttuple ts, subst
  | Evariant (tag, args, _) ->
    let arg_ts, subst = type_map subst scope args in
    Tvariant { fields = [tag, arg_ts]
             ; self = fresh_tvar ()
             ; more = fresh_tvar ()
             ; closed = false (* Open by default? *)
             }, subst
  | Eapply (f, x, _) -> begin
      match type_of_expr subst scope f with
      | Tarrow (a, b), subst ->
        let subst = unify (get_expr_type x) a subst x in
        b, subst
      | t, subst ->
        let a = fresh_tvar () and b = fresh_tvar () in
        let subst = unify (Tarrow (a, b)) t subst f in
        let subst = unify (get_expr_type x) a subst x in
        b, subst
    end
  | Efun (pat, e, _) ->
    let e_ts, subst = type_of_expr subst scope e in
    List.fold_right (fun pat t -> Tarrow (type_of_pattern pat, t)) pat e_ts, subst
  | Ematch (x, pats, _) as m -> begin
      let x_t = get_expr_type x
      and t = fresh_tvar () in
      let rec loop = function
        | [], subst -> subst
        | (pat, e)::branches, subst ->
          scope#enter_scope;
          bind_pattern scope pat;
          let subst = unify (type_of_pattern pat) x_t subst x in
          let e_t, subst = type_of_expr subst scope e in
          let subst = unify e_t t subst m in
          scope#leave_scope;
          loop (branches, subst) in
      let subst = loop (pats, subst) in
      t, subst
    end
  | Ebind (pat, e, ctx, _) ->
    scope#enter_scope;
    bind_pattern scope pat; (* TODO: unify with type_of e *)
    let t, subst = type_of_expr subst scope ctx in
    scope#leave_scope;
    t, subst
  | _ -> fresh_tvar (), subst
