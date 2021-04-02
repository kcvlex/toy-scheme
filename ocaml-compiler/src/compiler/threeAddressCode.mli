type t =
  | Bind of string * value_type
  | Move of string * value_type
  | Branch of value_type * string
  | Jump of string
  | Return of value_type
and value_type =
  | Int of int
  | Bool of bool
  | Ref of string
  | Label of string
  | Load of string * int
  | Quote of AstType.t
