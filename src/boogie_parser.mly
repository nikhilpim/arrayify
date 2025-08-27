/* ---------- Header ---------- */
%{
open Boogieir
open Variable
%}

/* ---------- Tokens ---------- */
%token ASSUME ASSERT RETURN IF ELSE
%token TRUE FALSE NOT AND OR
%token PLUS MINUS TIMES EQ LEQ
%token READ WRITE ROTATE
%token ASSIGN LPAR RPAR LBRACE RBRACE SEMI COMMA
%token <int> INT_LIT
%token <string> IDENT
%token EOF
%token PROCEDURE RETURNS COLON

/* ---------- Entry Point ---------- */
%start <BGraph.t> program
%type <(string * string list * string list * boogie_instr list) list> procedure_list
%type <string * string list * string list * boogie_instr list> procedure
%%

/* ---------- Grammar ---------- */

program:
  | procedure_list EOF { build_graph $1 }

procedure_list:
  | /* empty */ { [] }
  | procedure procedure_list { $1 :: $2 }

procedure:
  | PROCEDURE IDENT LPAR param_list RPAR RETURNS LPAR param_list RPAR LBRACE instr_list RBRACE
      { ($2, $4, $8, $11) }

param_list:
  | /* empty */ { [] }
  | IDENT COLON type_expr
      { [$1] }
  | IDENT COLON type_expr COMMA param_list
      { $1 :: $5 }

type_expr:
  | IDENT { $1 }   /* Just capture type as a string for now */

instr_list:
  | /* empty */ { [] }
  | instr instr_list { $1 :: $2 }

instr:
  | IDENT ASSIGN expr SEMI
      { Assign (boogie_var_of_var (new_var $1), $3) }
  | ASSUME formula SEMI
      { Assume ($2) }
  | ASSERT formula SEMI
      { Assert ($2) }
  | RETURN expr SEMI
      { Return ($2) }
  | IF LPAR formula RPAR LBRACE instr_list RBRACE ELSE LBRACE instr_list RBRACE
      { 
        (* Convert if-else into IteAssign or branch graph, but for now keep both lists *)
        (* We'll handle graph conversion later *)
        (* You might store as a pseudo-instruction or split later *)
        Error (* Placeholder; we will replace with graph conversion later *)
      }
  | WRITE LPAR IDENT COMMA expr COMMA expr RPAR SEMI
      { AWrite (new_avar $3, $5, $7) }
  | ROTATE LPAR rotate_list RPAR SEMI
      { Rotate ($3) }

rotate_list:
  | IDENT COMMA IDENT { [(new_avar $1, new_avar $3)] }
  | IDENT COMMA IDENT COMMA rotate_list { (new_avar $1, new_avar $3) :: $5 }

/* ---------- Expressions (boogie_term) ---------- */

expr:
  | INT_LIT { Int $1 }
  | IDENT { Var (boogie_var_of_var (new_var $1)) }
  | expr PLUS expr { Sum($1, $3) }
  | INT_LIT TIMES expr { Times($1, $3) }
  | READ LPAR IDENT COMMA expr RPAR { Read(new_avar $3, $5) }
  | LPAR expr RPAR { $2 }

/* ---------- Formulas (boogie_formula) ---------- */

formula:
  | TRUE { True }
  | FALSE { Not True }   /* Represent false as Not True */
  | expr EQ expr { Eq($1, $3) }
  | expr LEQ expr { Leq($1, $3) }
  | formula AND formula { And($1, $3) }
  | formula OR formula { Or($1, $3) }
  | NOT formula { Not($2) }
  | LPAR formula RPAR { $2 }
