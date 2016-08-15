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

let rec env_diff a b = match a, b with
  | EmptyEnv, _ -> EmptyEnv
  | a, EmptyEnv -> a
  | ExtendEnv (a, id, t), b when a = b -> ExtendEnv (EmptyEnv, id, t)
  | ExtendEnv (a, id, t) as sa, ExtendEnv (b, _, _) -> ExtendEnv (env_diff a b, id, t)

let rec env_join a b = match a with
  | EmptyEnv -> b
  | ExtendEnv (a, id, t) -> ExtendEnv (env_join a b, id, t)

let (-!) = env_diff and (+!) = env_join

let rec string_of_env = function
  | EmptyEnv -> ""
  | ExtendEnv (EmptyEnv, id, t) ->
    Id.string_of_ident id ^ " : " ^ Type.string_of_type t
  | ExtendEnv (e, id, t) ->
    Id.string_of_ident id ^ " => " ^ Type.string_of_type t ^ "\n" ^ string_of_env e

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
