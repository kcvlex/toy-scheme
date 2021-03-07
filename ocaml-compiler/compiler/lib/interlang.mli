type t =
  | Select of record_sym * int
  | Ref of inner_sym
  | Lambda of lambda_args * t * make_record
  | Branch of t * t * t
  | Apply of t * t * t list
  | Fix of (string * t) list * t
  | Int of int
  | Bool of bool
and record_sym = Record of int
and cont_sym = Cont of int
and lambda_args = Args of cont_sym option * record_sym * string list
and inner_sym =
  | RecordSym of record_sym
  | ContSym of cont_sym
  | Primitive of string
  | CommonSym of string
and make_record = record_sym * t list
