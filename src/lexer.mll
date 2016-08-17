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
      { inline_comment lexbuf }
  | "{*"
      { comment 0 lexbuf }
  | '"'
      { string_literal ~escaped:false ~str:"" lexbuf }
  | digit+ as digits
      { INT (int_of_string digits) }
  | digit+ '.' digit* as f | '.' digits+ as f
      { FLOAT (float_of_string f) }
  | "type"
      { TYPE }
  | "dual"
      { DUAL }
  | "open"
      { OPEN }
  | "val"
      { VAL }
  | "let"
      { LET }
  | "in"
      { IN }
  | "do"
      { DO }
  | "begin"
      { BEGIN }
  | "trace"
      { TRACE }
  | "match"
      { MATCH }
  | "with"
      { WITH }
  | "end"
      { END }
  | "if"
      { IF }
  | "then"
      { THEN }
  | "else"
      { ELSE }
  | lower_ident as l
      { LIDENT l }
  | upper_ident as u
      { UIDENT u }

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

and string_literal ~escaped ~str = parse
  | '\\'
      { if not escaped then
          string_literal ~escaped:true ~str lexbuf
        else
          string_literal ~escaped:false ~str:(str ^ "\\") lexbuf }
  | '"'
      { if not escaped then
          STRING str
        else
          string_literal ~escaped:false ~str:(str ^ "\"") lexbuf }
  | c
      { if not escaped then
          string_literal ~escaped ~str:(str ^ c) lexbuf
        else
          let escape = function
            | 't' -> "\t"
            | 'r' -> "\r"
            | 'n' -> "\n"
            | _ ->
              print_endline "Encountered unknown escape code in string!";
              assert false in
          string_literal ~escaped ~str:(str ^ escaped c) lexbuf }
