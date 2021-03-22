type t =
  | Apply of t * t * t list
  | Select of SymbolType.t * int
  | Ref of SymbolType.t
  | Branch of t * t * t
  | Int of int
  | Bool of bool
  | Closure of proc_env_pair
  | Program of procedure list * t
and procedure = {
  psym : SymbolType.t;
  args : proc_args;
  record : SymbolType.record_sym * t list;
  body : t;
}
and proc_args = {
  cont_arg : SymbolType.t option;
  closure_arg : SymbolType.closure_sym;
  usr_args : SymbolType.t list;
}
and proc_env_pair = {
  proc : t;
  env : SymbolType.t;
}
