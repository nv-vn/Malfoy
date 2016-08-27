type env = EmptyEnv | ExtendEnv of env * Id.ident * Type.t

let rec lookup env id =
  match env with
  | EmptyEnv -> None
  | ExtendEnv (_, id', ty)
    when id = id' -> Some ty
  | ExtendEnv (env, _, _) -> lookup env id

let rec unsafe_lookup env id =
  match lookup env id with
  | None ->
    print_endline @@ "Value `" ^ Id.string_of_ident id ^ "' is unbound in this environment";
    assert false
  | Some ty -> ty

let rec doesn't_occur tvar ty =
  let open Type in
  let all f = List.fold_left (fun b x -> (f x) && b) true in
  match ty with
  | Tvar a as tvar' -> tvar <> tvar'
  | Tarrow (a, b) -> doesn't_occur tvar a && doesn't_occur tvar b
  | Ttuple ts -> all (doesn't_occur tvar) ts
  | Tapply (a, b) -> doesn't_occur tvar a && doesn't_occur tvar b
  | Tvariant row -> all (fun (_, ts) -> all (doesn't_occur tvar) ts) row.fields
  | Ttag t -> doesn't_occur tvar t
  | Tdual t -> doesn't_occur tvar t
  | Tforall (_, t) -> doesn't_occur tvar t
  | Tconst _ -> true

let occurs tvar ty = not (doesn't_occur tvar ty)

let rec contains env t =
  match env with
  | EmptyEnv -> false
  | ExtendEnv (_, _, ty) when occurs t ty -> true
  | ExtendEnv (env, _, _) -> contains env t

let rec env_diff a b = match a, b with
  | EmptyEnv, _ -> EmptyEnv
  | a, EmptyEnv -> a
  | ExtendEnv (a, id, t), b when a = b -> ExtendEnv (EmptyEnv, id, t)
  | ExtendEnv (a, id, t), ExtendEnv (b, _, _) -> ExtendEnv (env_diff a b, id, t)

let rec env_join a b = match a with
  | EmptyEnv -> b
  | ExtendEnv (a, id, t) -> ExtendEnv (env_join a b, id, t)

let (-!) = env_diff and (+!) = env_join

let rec string_of_env = function
  | EmptyEnv -> ""
  | ExtendEnv (EmptyEnv, id, t) ->
    Id.string_of_ident id ^ " : " ^ Type.string_of_type t
  | ExtendEnv (e, id, t) ->
    Id.string_of_ident id ^ " : " ^ Type.string_of_type t ^ "\n" ^ string_of_env e

let rec env_of_list ?(env = EmptyEnv) = function
  | [] -> env
  | (id, t)::defs -> env_of_list ~env:(ExtendEnv (env, id, t)) defs

let default =
  let open Type in
  let int = !!(Id.Iident "Int")
  and string = !!(Id.Iident "String")
  and unit = !!(Id.Iident "()")
  and list a =
    Tapply (!!(Id.Iident "list"), a) in
  [ Id.Iident "::", ??"a" @-> list ??"a" @-> list ??"a"
  ; Id.Iident "+", int @-> int @-> int
  ; Id.Iident "-", int @-> int @-> int
  ; Id.Iident "*", int @-> int @-> int
  ; Id.Iident "/", int @-> int @-> int
  ; Id.Iident "print", string @-> unit
  ]
