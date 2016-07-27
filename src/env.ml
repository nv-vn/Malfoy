type ('a, 'b) generation =
  | Genesis
  | Grow of generation * ('a, 'b) Hashtbl.t

class env initial = object
  val mutable store = Hashtbl.create 100
  val mutable restore = Genesis

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

  initializer
    for i = 0 to Array.length initial do
      match initial.(i) with
      | (ident, ty) -> Hashtbl.add store ident ty
    done;
    restore <- Grow (restore, Hashtbl.copy store)
end

let default =
  let int = Tconst (Ident.Iident "Int")
  and list a =
    Tapply (Tconst (Ident.Iident "list"), a) in
  [| Ident.Iident "::", Tarrow (Tvar "a", Tarrow (list (Tvar "a"), list (Tvar "a")))
  ;  Ident.Iident "+", Tarrow (int, Tarrow (int, int))
  |]
