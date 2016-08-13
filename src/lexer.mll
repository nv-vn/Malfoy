{
open Parser
}

let ident_chars = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']+
let lower_ident = ['a'-'z' '_' '\''] ident_chars
let upper_ident = ['A'-'Z'] ident_chars

let digit = ['0'-'9']

rule token = parse
  | [' ' '\t' '\n' '\r']
      { token lexbuf }
  | "--"
      inline_comment lexbuf
  | "{*"
      comment 0 lexbuf

and comment depth = parse
  | "{*"
      { comment (succ depth) lexbuf }
  | "*}"
      { if depth = 0 then
          token lexbuf
        else
          comment (pred depth) lexbuf }
  | _
      { comment depth lexbuf }

and inline_comment = parse
  | "\r" | "\n"
      { token lexbuf }
  | _
      { inline_comment lexbuf }
