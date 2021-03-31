(* FIXME : MakeClosure instead of "list" *)

type t = {
  procs : (string * clsr_proc) list;
  body : clsr_expr;
} 
and clsr_proc = string * string list * string option * clsr_expr
and clsr_expr =
  | Term of clsr_term
  | Bind of (string * clsr_expr) list * clsr_expr
  | Branch of clsr_term * clsr_expr * clsr_expr
  | Call of clsr_term * clsr_term list
and clsr_term =
  | Int of int
  | Bool of bool
  | Primitive of string
  | Closure of string * clsr_term list
  | Var of string
  | Nil
  | Quote of AstType.t
  | MakeBox of string
  | RefBox of string
  | ClosureRef of clsr_term * int
