type t =
  | AdmLambda of SymbolType.t * t
  | ApplyFunc of t * t * t list
  | Passing of t * t
  | Fix of bind list * t
  | Int of int
  | Bool of bool
  | Ref of SymbolType.t
  | Lambda of SymbolType.t * SymbolType.t list * t
  | Branch of t * t * t
and bind = Bind of { sym : string; body : t; }
