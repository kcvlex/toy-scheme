type t =
  | Apply of t * t * t list
  | Select of SymbolType.t * int
  | Int of int
  | Bool of bool
  | Ref of SymbolType.t
  | Branch of t * t * t
  | Lambda of lambda_args * closure_record * t
and lambda_args = {
  cont_arg : SymbolType.t option;
  clo_arg : SymbolType.closure_sym;
  args : SymbolType.t list;
  passed_record : SymbolType.t list;
}
and closure_record = {
  rsym : SymbolType.record_sym;
  seq : record_element list;
}
and record_element = {
  sym : SymbolType.t;
  body : t;
  index : int;
  local : bool;
}
