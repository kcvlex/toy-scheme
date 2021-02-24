type t =
  | Value of cps_adm_sym * cps_value
  | Cont of cps_cont
  | PassCont of t * cps_cont
  | ApplyFunc of cps_sym * cps_cont * cps_sym list
  | Bind of cps_sym * cps_sym * t
and cps_sym =
  | UserSym of string
  | Primitive of string
  | AdmSym of cps_adm_sym
and cps_adm_sym =
  | AdmCont of int
  | AdmParam of int
and cps_cont =
  | ContSym of cps_adm_sym
  | AdmLambda of cps_adm_sym * t
and cps_value =
  | Int of int
  | Bool of bool
  | Sym of cps_sym
  | Lambda of cps_adm_sym * cps_sym list * t
