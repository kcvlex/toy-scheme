type t =
  | Term of term_type
  | Bind of bind_type list * t
  | Branch of term_type * t * t
  | Call of term_type * term_type list
and term_type =
  | Int of int
  | Bool of bool
  | Primitive of SymbolType.primitive_sym
  | Ref of string
  | Lambda of string list * string option * t
  | Nil
  | Quote of AstType.t
and bind_type =
  | BindValue of string * term_type
  | BindCall of string * term_type * term_type list
