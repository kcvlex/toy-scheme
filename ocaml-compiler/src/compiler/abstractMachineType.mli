type t =
  | Bind of string * value_type
  | Move of string * value_type
  | Test of value_type * t list * t list
  | Return
  | Call of value_type * value_type list
and value_type =
  | Int of int
  | Bool of bool
  | Primitive of SymbolType.primitive_sym
  | Label of int
  | Ref of string
  | Allocate of value_type list
  | RA
  | Nil
  | Quote of AstType.t
  | Cons of value_type * value_type
  | AccessClosure of string * int list
