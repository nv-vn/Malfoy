let (>>=) xs f = List.map f xs |> List.flatten

let maybe y = function Some x -> x
                     | None   -> y

let unsafe_get = function Some x -> x
                        | _      -> assert false
