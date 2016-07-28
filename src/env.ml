open Types

type ('a, 'b) generation =
  | Genesis
  | Grow of generation * (Ident.ident, Types.t) Hashtbl.t

class env initial = object (self)
  val mutable store = Hashtbl.create 100
  val mutable restore = Genesis
  val mutable uniq = "a"

  method lookup ident =
    if Hashtbl.mem store ident then
      Some (Hashtbl.find store ident)
    else
      None

  method enter_scope =
    restore <- Grow (restore, Hashtbl.copy store)

  method leave_scope =
    match restore with
    | Genesis ->
      store <- Hashtbl.clear store
    | Grow (last, g) -> begin
        restore <- last;
        store <- g
      end

  method register name ty =
    Hashtbl.add store name ty

  method register_unique ?(prefix="") ty =
    let next_uniq () =
      match uniq.[String.length uniq - 1] with
      | 'a' .. 'y' as c ->
        let last' = Char.chr (Char.code c + 1) in
        uniq.[String.length uniq - 1] <- last'
      | _ -> uniq <- uniq ^ "a" in
    let name = prefix ^ uniq in
    self#register name ty;
    next_uniq ();
    name

  initializer
    for i = 0 to Array.length initial do
      match initial.(i) with
      | (ident, ty) -> Hashtbl.add store ident ty
    done;
    restore <- Grow (restore, Hashtbl.copy store)
end

let default =
  let int = !!(Ident.Iident "Int")
  and string = !!(Ident.Iident "String")
  and unit = !!(Ident.Iident "()")
  and list a =
    Tapply (!!(Ident.Iident "list"), a) in
  [| Ident.Iident "::", ??"a" @-> list ??"a" @-> list ??"a"
  ;  Ident.Iident "+", int @-> int @-> int
  ;  Ident.Iident "-", int @-> int @-> int
  ;  Ident.Iident "*", int @-> int @-> int
  ;  Ident.Iident "/", int @-> int @-> int
  ;  Ident.Iident "print", string @-> unit
  |]
