type t =
  | AdmLambda of Cps.cps_sym * t
  | ApplyFunc of t * t * t list
  | Passing of t * t
  | Fix of bind list * t
  | Int of int
  | Bool of bool
  | Ref of Cps.cps_sym
  | RefIndex of int
  | Branch of t * t * t
  | Lambda of Cps.cps_sym * Cps.cps_sym list * t * int
and bind = Bind of { sym : string; body : t; }
