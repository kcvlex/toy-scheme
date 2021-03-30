type t =
  | Term of term_type
  | Bind of (string * t) list * t
  | Branch of term_type * t * t
  | TailCall of term_type * term_type list
and term_type =
  | Int of int
  | Bool of bool
  | Primitive of string
  | Ref of string
  | Lambda of string list * string option * t
  | Nil
  | Quote of AstType.t
