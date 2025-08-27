{
open Boogie_parser
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | "assume"   { ASSUME }
  | "assert"   { ASSERT }
  | "return"   { RETURN }
  | "if"       { IF }
  | "else"     { ELSE }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "not"      { NOT }
  | "and"      { AND }
  | "or"       { OR }
  | "read"     { READ }
  | "write"    { WRITE }
  | "rotate"   { ROTATE }
  | ":="       { ASSIGN }
  | '='        { EQ }
  | "<="       { LEQ }
  | '+'        { PLUS }
  | '*'        { TIMES }
  | '('        { LPAR }
  | ')'        { RPAR }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | ';'        { SEMI }
  | ','        { COMMA }
  | ['0'-'9']+ as i { INT_LIT (int_of_string i) }
  | ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as id { IDENT id }
  | "procedure" { PROCEDURE }
  | "returns"   { RETURNS }
  | ":"         { COLON }
  | eof        { EOF }
