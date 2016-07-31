open Type

type generation =
  | Genesis
  | Grow of generation * (Id.ident, Type.t) Hashtbl.t

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
      Hashtbl.clear store
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
    self#register (Id.Iident name) ty;
    next_uniq ();
    name

  initializer
    for i = 0 to Array.length initial - 1 do
      match initial.(i) with
      | (ident, ty) -> Hashtbl.add store ident ty
    done;
    restore <- Grow (restore, Hashtbl.copy store)
end

let default =
  let int = !!(Id.Iident "Int")
  and string = !!(Id.Iident "String")
  and unit = !!(Id.Iident "()")
  and list a =
    Tapply (!!(Id.Iident "list"), a) in
  [| Id.Iident "::", ??"a" @-> list ??"a" @-> list ??"a"
  ;  Id.Iident "+", int @-> int @-> int
  ;  Id.Iident "-", int @-> int @-> int
  ;  Id.Iident "*", int @-> int @-> int
  ;  Id.Iident "/", int @-> int @-> int
  ;  Id.Iident "print", string @-> unit
  |]
