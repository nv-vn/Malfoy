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
      { string_literal false "" lexbuf }
  | digit+ as digits
      { INT (int_of_string digits) }
  | digit+ '.' digit* as f | '.' digit+ as f
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
  | '_'
      { UNDERSCORE }
  | lower_ident as l
      { LIDENT l }
  | upper_ident as u
      { UIDENT u }
  | '('
      { OPEN_PAREN }
  | ')'
      { CLOSE_PAREN }
  | ':'
      { COLON }
  | "::"
      { COLON_COLON }
  | ','
      { COMMA }
  | '\\' (* Add actual lambda character *)
      { LAMBDA }
  | "->" (* Add actual arrow character *)
      { RIGHT_ARROW }
  | '|'
      { BAR }
  | eof
      { EOF }

and comment depth = parse
  | "{*"
      { comment (succ depth) lexbuf }
  | "*}"
      { if depth = 0 then
          token lexbuf
        else
          comment (pred depth) lexbuf }
  | eof
      { EOF }
  | _
      { comment depth lexbuf }

and inline_comment = parse
  | "\r" | "\n"
      { token lexbuf }
  | eof
      { EOF }
  | _
      { inline_comment lexbuf }

and string_literal escaped str = parse
  | '\\'
      { if not escaped then
          string_literal true str lexbuf
        else
          string_literal false (str ^ "\\") lexbuf }
  | '"'
      { if not escaped then
          STRING str
        else
          string_literal false (str ^ "\"") lexbuf }
  | eof
      { EOF }
  | _ as c
      { if not escaped then
          string_literal escaped (str ^ c) lexbuf
        else
          let escape = function
            | 't' -> "\t"
            | 'r' -> "\r"
            | 'n' -> "\n"
            | _ ->
              print_endline "Encountered unknown escape code in string!";
              assert false in
          string_literal escaped (str ^ escaped c) lexbuf }
