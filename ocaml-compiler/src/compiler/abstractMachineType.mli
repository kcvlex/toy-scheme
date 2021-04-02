type t =
  | Bind of string * value_type
  | Move of string * value_type
  | Test of value_type * t list * t list
  | Return
  | Value of value_type
  | Call of value_type * value_type list
and value_type =
  | Int of int
  | Bool of bool
  | Primitive of SymbolType.primitive_sym
  | Label of string
  | Ref of string
  | Allocate of value_type list
  | RA
  | Nil
  | Quote of AstType.t
  | Cons of value_type * value_type
  | AccessClosure of string * int list
and bblock_type = t list
and proc_type = string option * string list * string option * bblock_type
