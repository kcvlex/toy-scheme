type t =
  | Term of term_type
  | Bind of (string * term_type) * t
  | Branch of term_type * t * t
  | TailCall of term_type * term_type list
  | Call of (string * term_type * term_type list) * t
and term_type =
  | Int of int
  | Bool of bool
  | Primitive of string
  | Ref of string
  | Lambda of string list * string option * t
