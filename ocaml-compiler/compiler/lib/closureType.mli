type t =
  | Apply of t * t * t list
  | Fix of bind list * t
  | Select of int
  | Int of int
  | Bool of bool
  | Ref of SymbolType.t
  | Branch of t * t * t
  | Lambda of SymbolType.t option * SymbolType.t * SymbolType.t list * t * clo_record * make_record
and bind = Bind of { sym : string; body : t; }
and clo_record = SymbolType.t list
and make_record = (SymbolType.t * t option) list
