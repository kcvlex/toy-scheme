type t =
  | Apply of t * t * t list
  | Fix of bind list * t
  | Select of int
  | Int of int
  | Bool of bool
  | Ref of clo_sym
  | Branch of t * t * t
  | Lambda of clo_sym option * clo_sym * clo_sym list * t * clo_record * make_record
and clo_sym =
  | UserSym of string
  | Primitive of string
  | ContSym of int
  | ParamSym of int
  | ClosureSym of int
and bind = Bind of { sym : string; body : t; }
and clo_record = clo_sym list
and make_record = (clo_sym * t option) list
