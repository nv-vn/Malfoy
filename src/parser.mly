%{
  open Substs

  type stmt = [>] (* Too much to define, but needs to be named *)
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TYPE DUAL OPEN VAL LET IN DO BEGIN TRACE MATCH WITH END IF THEN ELSE
%token UNDERSCORE
%token <string> LIDENT UIDENT
%token OPEN_PAREN CLOSE_PAREN COLON COLON_COLON COMMA LAMBDA RIGHT_ARROW BAR
%token EOF

%start statements
%type <stmt> statements

%%

statements:
  | EOF
    { [] }
  | statement statements
    { $1::$2 }
;

statement:
  | DO expr END
    { `Sexec $2 }
  | VAL LIDENT COLON type
    { `Sval ($2, $4) }
;

expr: /* EMPTY */ {}
;

type: /* EMPTY */ {}
;