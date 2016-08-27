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
      | Some value -> subst value
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
  | Ttag t -> Ttag (subst t)
  | Tdual t -> Tdual (subst t)
  | Tforall (ts, t) -> Tforall (ts, apply_substitutions t substs)
  | ty -> ty

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
  | Tforall _, _ | _, Tforall _ -> assert false (* Will this case ever exist during [unify] *)
  | _, _ ->
    print_endline ("Couldn't unify types: \n  " ^ string_of_type t1 ^ "\nand\n  " ^ string_of_type t2);
    assert false (* Can't unify types *)

let type_of_literal lit =
  match lit with
  | Lchar _ -> char
  | Lstring _ -> string
  | Lint _ -> int
  | Lfloat _ -> float

(* TODO: Should this perform some unification? Should it just return the assigned type? *)
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

let rec bind_patterns ?(subst=EmptySubst) ?(free=[]) env =
  let make_free t = (* Automatically remove unused quantified types *)
    let free' =
      free >>= fun t' ->
      let t'' = apply_substitutions t' subst in
      if occurs t'' (apply_substitutions t subst) then [t''] else [] in
    if free' = [] then t
    else Tforall (free', t) in
  function
  | Pident (id, t) -> ExtendEnv (env, Id.Iident id, make_free t)
  | Ptuple (ps, _) -> List.fold_left (bind_patterns ~subst ~free) env ps
  | Pvariant (_, args, _) -> List.fold_left (bind_patterns ~subst ~free) env args
  | _ -> env

let rec generalize gamma = function
  | Tconst _ -> []
  | Tarrow (a, b) -> generalize gamma a @ generalize gamma b
  | Ttuple ts -> ts >>= generalize gamma
  | Tapply (t, t') -> generalize gamma t @ generalize gamma t'
  | Tvariant { fields; self; more } -> fields >>= fun (_, ts) -> ts >>= generalize gamma
  | Ttag t -> generalize gamma t
  | Tdual t -> generalize gamma t
  | t -> if contains gamma t then [] else [t] (* If it's already contained in G then it's bound *)

let instantiate bound =
  let subst = Hashtbl.create 10 in
  let rec inst = function
    | Tconst _ as ty -> ty
    | Tarrow (a, b) -> Tarrow (inst a, inst b)
    | Ttuple ts -> Ttuple (List.map inst ts)
    | Tapply (t, t') -> Tapply (inst t, inst t')
    | Tvariant row ->
      let self' = fresh_tvar ()
      and more' = fresh_tvar () in
      Hashtbl.add subst row.self self';
      Hashtbl.add subst row.more more';
      Tvariant { row with self = self';
                          more = more';
                          fields = List.map (fun (tag, ts) -> (tag, List.map inst ts)) row.fields }
    | Ttag t -> Ttag (inst t)
    | Tdual t -> Tdual (inst t)
    | Tvar a as ty when Hashtbl.find_all subst ty = [] ->
      if List.mem ty bound then
        let ty' = fresh_tvar () in
        Hashtbl.add subst ty ty';
        ty'
      else ty
    | Tvar a as ty -> Hashtbl.find subst ty
    | ty -> ty
  in inst

let rec collect_substitutions subst env = function
  | Eliteral (l, t) ->
    unify t (type_of_literal l) subst
  | Eident (id, t) ->
    unify t (unsafe_lookup env id) subst
  | Etuple (es, t) ->
    let ts = List.map get_expr_type es in
    let subst' = List.fold_left (fun subst e -> collect_substitutions subst env e) subst es in
    unify t (Ttuple ts) subst'
  | Evariant (tag, args, t) ->
    let arg_ts = List.map get_expr_type args in
    let subst' = List.fold_left (fun subst e -> collect_substitutions subst env e) subst args in
    let var = Tvariant { fields = [tag, arg_ts]
                       ; self = fresh_tvar ()
                       ; more = fresh_tvar ()
                       ; closed = true (* TODO: What is the "sane default" here? *)
                       } in
    unify t var subst'
  | Eapply (f, x, t) -> begin
      let f_t = get_expr_type f
      and x_t = get_expr_type x in
      let subst_f = collect_substitutions subst env f in
      let subst_x = collect_substitutions subst_f env x in
      let f_t' = match apply_substitutions f_t subst_x with
        | Tforall (ts, t) -> instantiate ts t
        | t -> t in
      let t' = fresh_tvar () in
      let subst' = unify f_t' (Tarrow (x_t, t')) subst_x in
      unify t t' subst'
    end
  | Efun (pat, e, t) ->
    let env' = bind_patterns env pat in
    let subst' = collect_substitutions subst env' e in
    unify t (Tarrow (type_of_pattern pat, get_expr_type e)) subst'
  | Ematch (x, branches, t) ->
    let x_t = get_expr_type x in
    List.fold_left
      (fun subst (pat, e) ->
         let subst' = unify x_t (type_of_pattern pat) subst in
         let env' = bind_patterns env pat in
         let subst'' = collect_substitutions subst' env' e in
         unify t (get_expr_type e) subst'')
      subst branches
  | Ebind (pat, e, ctx, t) ->
    let subst' = unify (get_expr_type e) (type_of_pattern pat) subst in
    let env' = bind_patterns env pat in
    let subst_e = collect_substitutions subst' env' e in (* TODO: is env' correct here? Should allow recursion *)
    let t_e = apply_substitutions (get_expr_type e) subst_e in
    let env'' = bind_patterns ~subst:subst_e ~free:(generalize env' t_e) env pat in (* Does [env] here cause old changes to drop? *)
    let subst_ctx = collect_substitutions subst_e env'' ctx in
    unify t (get_expr_type ctx) subst_ctx

let infer_types ?(subst=EmptySubst) ?(env=EmptyEnv) e =
  let subst' = collect_substitutions subst env e in
  let rec apply_pat = function
    | Pwildcard t -> Pwildcard (apply_substitutions t subst')
    | Pliteral (l, t) -> Pliteral (l, apply_substitutions t subst')
    | Pident (s, t) -> Pident (s, apply_substitutions t subst')
    | Ptuple (pats, t) -> Ptuple (List.map apply_pat pats, apply_substitutions t subst')
    | Pvariant (tag, args, t) -> Pvariant (tag, List.map apply_pat args, apply_substitutions t subst') in
  let rec apply = function
    | Eliteral (l, t) -> Eliteral (l, apply_substitutions t subst')
    | Eident (id, t) -> Eident (id, apply_substitutions t subst')
    | Etuple (es, t) -> Etuple (List.map apply es, apply_substitutions t subst')
    | Evariant (tag, args, t) -> Evariant (tag, List.map apply args, apply_substitutions t subst')
    | Eapply (f, x, t) -> Eapply (apply f, apply x, apply_substitutions t subst')
    | Efun (pat, e, t) -> Efun (apply_pat pat, apply e, apply_substitutions t subst')
    | Ematch (x, branches, t) ->
      let apply_branch (pat, e) = (apply_pat pat, apply e) in
      Ematch (apply x, List.map apply_branch branches, apply_substitutions t subst')
    | Ebind (pat, e, ctx, t) -> Ebind (apply_pat pat, apply e, apply ctx, apply_substitutions t subst')
  in apply e

let type_ast ?(subst=EmptySubst) ?(env=EmptyEnv) stmts =
  (* TODO: Extract type info from annotated top-level bindings! *)
  List.map (function Sexec e -> Sexec (infer_types ~subst ~env e)
                   | Sbind (pat, e) ->
                     let env = bind_patterns env pat in
                     Sbind (pat, infer_types ~subst ~env e)
                   | stmt -> stmt) stmts
