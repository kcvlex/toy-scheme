type t =
  | RecordSym of record_sym
  | ClosureSym of closure_sym  (* argument of lambda *)
  | ContSym of cont_sym
  | ParamSym of int
  | Primitive of string
  | CommonSym of string
  | ProcSym of proc_sym
and record_sym = Record of int
and cont_sym = Cont of int
and proc_sym = Proc of int
and closure_sym = Closure of int
