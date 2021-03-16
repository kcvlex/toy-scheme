type t =
  | RecordSym of record_sym
  | ContSym of cont_sym
  | ParamSym of int
  | Primitive of string
  | CommonSym of string
and record_sym = Record of int
and cont_sym = Cont of int
and proc_sym = Proc of int
