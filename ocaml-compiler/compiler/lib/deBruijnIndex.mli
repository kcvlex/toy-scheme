type t =
  | AdmLambda of Cps.cps_sym * t
  | ApplyFunc of t * t * t list
  | Passing of t * t
  | Bind of Cps.cps_sym * t * t
  | Int of int
  | Bool of bool
  | Ref of Cps.cps_sym
  | RefIndex of int
  | Lambda of Cps.cps_sym * Cps.cps_sym list * t * int
