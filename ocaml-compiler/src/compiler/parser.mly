%{
  open AstType
%}

%token LAMBDA DEFINE COND ELSE BEGIN LET
%token LPAREN RPAREN DOT EOF
%token <int> NUM
%token <string> ID

%start root
%type <AstType.t> root

%%

root: expr EOF { $1 } ;

(* Expr *)
expr:
  | define | binds | cond | lambda | stmt | apply | atom { $1 }
;

(* Define *)
define: LPAREN DEFINE ID expr RPAREN { { sym = $3; def = $4 } };

(* Binds *)
binds: LPAREN LET LPAREN list(bind) RPAREN expr RPAREN { Let ($4, $6) };
bind: LPAREN ID expr RPAREN { { sym = $2; def = $3 } };

(* Cond *)
cond: LPAREN COND cond_aux RPAREN { $3 };
cond_aux:
  | LPAREN expr expr RPAREN LPAREN ELSE expr RPAREN { Branch ($2, $3, $7) }
  | LPAREN expr expr RPAREN cond_aux { Branch ($2, $3, $5) }
;

(* Lambda *)
lambda: 
  | LPAREN LAMBDA args1 expr RPAREN { Lambda ($3, None $4) }
  | LPAREN LAMBDA args2 expr RPAREN { Lambda ((fst $3), (snd $3), $4) }
  | LPAREN LAMBDA ID expr RPAREN { Lambda ([], Some $3, $4) }
;
args1: LPAREN list(ID) RPAREN { $2 };
args2: LPAREN list(ID) DOT ID RPAREN { ($2, Some $4) };

(* Stmt *)
stmt: LPAREN BEGIN list(expr) RPAREN { Statement $3 };

(* Apply *)
apply: LPAREN expr list(expr) RPAREN { Apply ($2, $3) };

(* Atom *)
atom:
  | NUM { Num $1 }
  | ID {
    match $1 with
      | "+" | "-" | "*" | "/" | "eq?" | "set!" | "<" -> Primitive $1
      | "#t" -> Bool true
      | "#f" -> Bool false
      | _ -> Symbol $1 
    }
;

%%
