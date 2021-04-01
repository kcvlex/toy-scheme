%{
  open AstType
  open SymbolType
%}

%token LAMBDA DEFINE IF COND ELSE BEGIN LET
%token LPAREN RPAREN DOT QUOTE EOF
%token <string> NUM
%token <string> ID

%start root
%type <AstType.t> root

%%

root: expr EOF { $1 } ;

(* Expr *)
expr:
  | define | binds | branch | lambda | stmt | apply | atom { $1 }
;

(* Define *)
define: LPAREN DEFINE ID expr RPAREN expr { Define ([ ($3, $4) ], $6) };

(* Binds *)
binds: LPAREN LET LPAREN list(bind_one) RPAREN expr RPAREN { Let ($4, $6) };
bind_one: LPAREN ID expr RPAREN { ($2, $3) };

(* Branch *)
branch: b_cond | b_if { $1 };
b_cond: LPAREN COND b_cond_aux RPAREN { $3 };
b_cond_aux:
  | LPAREN expr expr RPAREN LPAREN ELSE expr RPAREN { Branch ($2, $3, $7) }
  | LPAREN expr expr RPAREN b_cond_aux { Branch ($2, $3, $5) }
;
b_if: LPAREN IF expr expr expr RPAREN { Branch ($3, $4, $5) };

(* Lambda *)
lambda: 
  | LPAREN LAMBDA args1 expr RPAREN { Lambda ($3, None, $4) }
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
  | LPAREN RPAREN { Nil }
  | NUM { Num (int_of_string $1) }
  | ID {
    match $1 with
      | "+"        -> Symbol (PrimitiveSym ADD)
      | "-"        -> Symbol (PrimitiveSym SUB)
      | "*"        -> Symbol (PrimitiveSym MUL)
      | "/"        -> Symbol (PrimitiveSym DIV)
      | "eq?"      -> Symbol (PrimitiveSym EQ)
      | "<"        -> Symbol (PrimitiveSym LESS)
      | "null?"    -> Symbol (PrimitiveSym NULL)
      | "cons"     -> Symbol (PrimitiveSym CONS)
      | "car"      -> Symbol (PrimitiveSym CAR)
      | "cdr"      -> Symbol (PrimitiveSym CDR)
      | "list"     -> Symbol (PrimitiveSym LIST)
      | "list-ref" -> Symbol (PrimitiveSym LISTREF)
      | "apply"    -> Symbol (PrimitiveSym APPLY)
      | "map"      -> Symbol (PrimitiveSym MAP)
      | "display"  -> Symbol (PrimitiveSym DISPLAY)
      | "#t"       -> Bool true
      | "#f"       -> Bool false
      | "()"       -> Nil
      | s          -> Symbol (CommonSym s)
    }
  | QUOTE expr { Quote $2 }
;

%%
