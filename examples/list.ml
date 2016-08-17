type List a = [< Nil
               | Cons a (List a)]

let length xs = match xs with
  | Nil -> 0
  | Cons (x, xs) -> 1 + length xs
  end
