type t =
  | Select of SymbolType.record_sym * int
  | Ref of SymbolType.t
  | Branch of t * t * t
  | Apply of t * t * t list
  | Int of int
  | Bool of bool
  | Let of (string * t) list * t
  | Closure of SymbolType.proc_sym * SymbolType.record_sym
  | Program of procedure list * t
and proc_args = Args of SymbolType.cont_sym option * SymbolType.record_sym option * string list
and procedure = Procedure of SymbolType.proc_sym * proc_args * make_record * t
and make_record = SymbolType.record_sym * t list
