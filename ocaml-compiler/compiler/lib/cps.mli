type t =
  | AdmLambda of cps_sym * t
  | ApplyFunc of t * t * t list
  | Passing of t * t
  | Bind of cps_sym * t * t
  | Int of int
  | Bool of bool
  | Ref of cps_sym
  | Lambda of cps_sym * cps_sym list * t
and cps_sym =
  | UserSym of string
  | Primitive of string
  | ContSym of int
  | ParamSym of int
