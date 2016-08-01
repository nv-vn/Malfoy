# Behavioural types

* Based on polymorphic variants (à la OCaml)
* Subtyped variant types are inferred based on usage
* Type-level duality
  + Higher-kinded structural duality (i.e. given the duals `I` and `O`, the dual of `type A = I Int` is `O Int` without being declared explicitly)
* The `-[...]` type allows for accessing the tags directly
* ```⁣`_``` in polymorphic variants to match against an unknown tag

## Session types example

```ocaml
data Start = Start
type Api = O Start (R [ `Ok (S [ `Status (I Bool Eps)
                               | `Uptime (I Int  Eps)])
                      | `Err Eps])

(* `Eps` corresponds to the epsilon type (which ends a session)
   The `O` type is an "output" (i.e. sending a message), while `I` refers to "input" (receiving the message).
   `S` is selection (internal choice) and `R` is reception (external choice).
   The syntax for variants is partially based on OCaml's syntax, but no `of` keyword is used. *)
```

The code for `Api` corresponds to:

![Api](https://latex.codecogs.com/gif.latex?%21%5Ctextup%7BStart%7D.%5C%26%5C%7B%5Ctextup%7BOk%7D%3A%20%5Coplus%5C%7B%5Ctextup%7BStatus%7D%3A%20%3F%5Ctextup%7BBool%7D%20.%20%5Cvarepsilon%2C%20%5Ctextup%7BUptime%7D%3A%3F%5Ctextup%7BInt%7D%20.%20%5Cvarepsilon%5C%7D%2C%20%5Ctextup%7BErr%7D%3A%20%5Cvarepsilon%5C%7D)

A matching implementation for the API defined above is given below:

```ocaml
(* client : Api -> Unit *)
let client ep =
  (* send : msg -> O msg cont -> cont *)
  let ep = send Start ep in
  (* branch : R ([>] as msg) -> msg *)
  match branch ep with
  | `Ok ep ->
    (* select : -([> `_ cont] as choice) -> S choice -> cont *)
    let ep = select `Status ep in
    (* receive : I msg cont -> msg * cont *)
    let status, ep = receive ep in
    (* close : Eps -> Unit *)
    close ep
  | `Err ep -> close ep
  end
```

## Resource type example

...

## Duality example

```ocaml
type Client = SomeClientType
type Server = SomeServerType

dual Client <=> Server

(* Given [cospawn : (client -> Unit) -> (server -> Unit) -> Unit
                    constraint server = Dual client] ... *)

(* Server/Client arguments represent the session resource that gets consumed *)
val myClient : Client -> Unit
let myClient = (* ... *)

val myServer : Server -> Unit
let myServer = (* ... *)

let () = cospawn myClient myServer

(* Given [spawnPid : (process -> Unit) -> Dual process] *)

let () = myServer (spawnPid myClient)
```

# Proposed syntax changes

* Maybe stick with OCaml-style syntax for type names/variables, so we don't need to figure out if `type A = A` means a circular type definition or a variant type with the constructor `A`?
* Not a fan of `-` for tag-only variants, maybe `~` or `@` or something along those lines would be better?
  + Use `'` for the tags themselves, i.e. `'Abc` is a tag. Can be an operator too?
* It should be significantly easier to express session types/behavioural types than in other languages, but how can we get that? Type-level operators with unicode symbols might be a good start to getting more "math-y" definitions, but is that a real gain?
* `as` syntax in types is kind of wonky... should we have a better way to reduce ambiguity than parens?
* Should we accept single-line, Haskell-style comments (`-- ...`)?
