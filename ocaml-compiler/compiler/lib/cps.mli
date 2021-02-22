type t =
  | Program of cps_cont
  | Cont of cps_cont
  | PassCont of t * cps_cont
  | CallCont of cps_cont * t
  | Triv of cps_triv
and cps_triv =
  | Int of int
  | Bool of bool
  | Sym of cps_sym
  | Lambda of cps_sym * t
  | Def of cps_sym * t
  | Cons of t * t
and cps_cont =
  | ContSym of cps_sym
  | ContFunc of cps_sym * t
and cps_sym =
  | UserSym of string
  | CpsContSym of int
  | CpsParamSym of int
