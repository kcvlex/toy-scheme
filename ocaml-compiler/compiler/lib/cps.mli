type t =
  | AdmLambda of cps_sym * t
  | ApplyFunc of cps_sym * cps_sym * cps_sym list
  | PassCont of t * t
  | CallCont of t * t
  | Sym of cps_sym
  | Bind of cps_sym * cps_sym * t
  | Value of cps_sym * cps_value
and cps_sym =
  | UserSym of string
  | Primitive of string
  | ContSym of int
  | ParamSym of int
  | GeneratedSym of int
and cps_value =
  | Int of int
  | Bool of bool
  | Ref of cps_sym
  | Lambda of cps_sym * cps_sym list * t
