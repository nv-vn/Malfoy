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

![Api](https://latex.codecogs.com/gif.latex?\inline%5Cinline%26space%3B!%5Ctextup%7BStart%7D.%26%5C%7B%5Ctextup%7BOk%7D%3A%26space%3B%5Coplus%5C%7B%5Ctextup%7BStatus%7D%3A%26space%3B%3F%5Ctextup%7BBool%7D%26space%3B.%26space%3B%5Cvarepsilon%2C%26space%3B%5Ctextup%7BUptime%7D%3A%3F%5Ctextup%7BInt%7D%26space%3B.%26space%3B%5Cvarepsilon%5C%7D%2C%26space%3B%5Ctextup%7BErr%7D%3A%26space%3B%5Cvarepsilon%5C%7D)

