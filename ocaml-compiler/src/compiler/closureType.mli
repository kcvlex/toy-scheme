type t = {
  procs : (string * clsr_proc) list;
  body : clsr_expr;
} 
and clsr_proc = string * string list * string option * clsr_expr
and clsr_expr =
  | Term of clsr_term
  | Bind of (string * clsr_term) list * clsr_expr
  | Branch of clsr_term * clsr_expr * clsr_expr
and clsr_term =
  | Int of int
  | Bool of bool
  | Closure of string * clsr_term list
  | Primitive of SymbolType.primitive_sym
  | Call of clsr_term * clsr_term list
  | Var of string
  | Nil
  | Quote of AstType.t
  | MakeBox of string
  | RefBox of string
  | ClosureRef of clsr_term * int
