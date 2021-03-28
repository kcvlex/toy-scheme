type t =
  | Int of int
  | Bool of bool
  | Nil
  | Quote of AstType.t
  | AdmLambda of SymbolType.t * t
  | Lambda of SymbolType.t * SymbolType.t list * SymbolType.t option * t
  | ApplyFunc of t * t * t list
  | ApplyPrimitive of SymbolType.t * t list
  | Passing of t * t
  | Let of (string * t) * t
  | Ref of SymbolType.t
  | Branch of t * t * t
  | RefIndex of int
