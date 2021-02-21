type t =
  | Program of cps_cont
  | PassCont of t * t list * cps_cont
  | CallCont of cps_cont * t
  | Triv of cps_triv
  | Def of string * t
and cps_triv =
  | Int of int
  | Bool of bool
  | Lit of string
  | Sym of string
  | Lambda of string list * cont_id * t
and cps_cont =
  | ContSym of cont_id
  | ContFunc of cont_id * t
and cont_id = string

