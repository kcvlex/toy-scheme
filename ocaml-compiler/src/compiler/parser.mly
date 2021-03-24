%{
  open AstType
%}

%token LAMBDA DEFINE COND ELSE
%token LPAREN RPAREN EOF
%token <string> SYMBOL
%token <int> NUM
%token <bool> BOOL

%start root
%type <AstType.t> root

%%

root: expr EOF { $1 } ;

(* Expr *)
expr: 
  | define expr {
      match $2 with
        | Define (lis, body) -> Define ($1 :: lis, body)
        | _ -> Define ([ $1 ], $2)
    }
  | term { $1 }

(* Define *)
define: LPAREN DEFINE SYMBOL expr RPAREN { Bind { sym = $3; def = $4 } };

term:
  | lambda { $1 }
  | cond { $1 }
  | apply { $1 }
  | atom { $1 }
;

(* Lambda *)
lambda: LPAREN LAMBDA args expr RPAREN { Lambda ($3, $4) };
args: LPAREN list(SYMBOL) RPAREN { $2 };

(* Apply *)
apply: LPAREN expr list(expr) RPAREN { Apply ($2, $3) };

(* Atom *)
atom:
  | NUM { Num $1 }
  | BOOL { Bool $1  }
  | SYMBOL { Symbol $1  }
;

(* Cond *)
cond: LPAREN COND cond_aux RPAREN { $3 };

cond_aux:
  | LPAREN expr expr RPAREN LPAREN ELSE expr RPAREN { Branch ($2, $3, $7) }
  | LPAREN expr expr RPAREN cond_aux { Branch ($2, $3, $5) }
;

%%
