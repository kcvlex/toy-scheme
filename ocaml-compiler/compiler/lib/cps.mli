type t =
  | AdmLambda of cps_sym * t
  | ApplyFunc of t * t * t list
  | Passing of t * t
  | Fix of bind list * t
  | Int of int
  | Bool of bool
  | Ref of cps_sym
  | Lambda of cps_sym * cps_sym list * t
  | Branch of t * t * t
and cps_sym =
  | UserSym of string
  | Primitive of string
  | ContSym of int
  | ParamSym of int
and bind = Bind of { sym : string; body : t; }
