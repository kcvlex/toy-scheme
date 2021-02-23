%{
  open Ast
%}

%token LAMBDA DEFINE
%token LPAREN RPAREN EOF
%token <string> SYMBOL
%token <int> NUM
%token <bool> BOOL

%start root
%type <Ast.t> root

%%

root: expr EOF { $1 } ;

(* Cons *)
expr:
  | term expr { Cons ($1, Some $2) }
  | term { Cons ($1, None) }
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
lambda: LAMBDA LPAREN args RPAREN expr { Lambda ($3, $5) };
args: list(SYMBOL) { List.map (fun s -> (Symbol s)) $1 };

(* Define *)
define: DEFINE SYMBOL expr { Define ((Symbol $2), $3) };

(* Apply *)
apply: term list(term) { Apply ($1, $2) };

(* Atom *)
atom:
  | NUM { Num $1 }
  | BOOL { Bool $1  }
  | SYMBOL { Symbol $1  }
;

%%
