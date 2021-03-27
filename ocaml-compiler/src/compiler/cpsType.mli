type t =
  | Int of int
  | Bool of bool
  | AdmLambda of SymbolType.t * t
  | Lambda of SymbolType.t * SymbolType.t list * SymbolType.t option * t
  | ApplyFunc of t * t * t list
  | Passing of t * t
  | Let of (string * t) * t
  | RefDirect of SymbolType.t
  | RefBox of SymbolType.t
  | MakeBox of t
  | Branch of t * t * t
  | RefIndex of int
