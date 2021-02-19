%{
  open Ast
%}

%token LAMBDA DEFINE
%token LPAREN RPAREN EOF
%token <string> SYMBOL
%token <int> NUM
%token <bool> BOOL

%start root
%type <Ast.ast_node> root

%%

root: expr EOF { $1 } ;

(* Cons *)
expr:
  | term expr { Cons($1, $2) }
  | term { $1 }
;

term:
  | LPAREN term_aux RPAREN { $2 }
  | atom { $1 }
;

term_aux:
  | lambda { $1 }
  | define { $1 }
  | apply { $1 }
;

(* Lambda *)
lambda: LAMBDA LPAREN args RPAREN expr { Lambda($3, $5) };
args: list(SYMBOL) { $1 };

(* Define *)
define: DEFINE SYMBOL expr { Define($2, $3) };

(* Apply *)
apply: expr list(expr) { Apply($1, $2) };

(* Atom *)
atom:
  | NUM { Num($1) }
  | BOOL { Bool($1) }
  | SYMBOL { Symbol($1) }
;

%%
