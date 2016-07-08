# Behavioural types

* Based on polymorphic variants (à la OCaml)
* Subtyped variant types are inferred based on usage
* The `-[...]` type allows for accessing the tags directly

```
type End = End
type Start = Start
type Api = O $ Start . R [ `Ok $ S [ `Status $ I Bool . End
                                   | `Uptime $ I Int  . End]
                         | `Err End]

-- The `O` type is an "output" (i.e. sending a message), while `I` refers to "input" (receiving the message).
-- `S` is selection (internal choice) and `R` is reception (external choice).
-- The `$` operator simply acts in place of parentheses and `.` composes types such that the right hand side is the continuation of the session.
-- The syntax for variants is partially based on OCaml's syntax, but no `of` keyword is used.
```

The code for `Api` corresponds to:

![Api](https://latex.codecogs.com/gif.latex?\inline&space;!\textup{Start}.&\{\textup{Ok}:&space;\oplus\{\textup{Status}:&space;?\textup{Bool}&space;.&space;\varepsilon,&space;\textup{Uptime}:?\textup{Int}&space;.&space;\varepsilon\},&space;\textup{Err}:&space;\varepsilon\})

