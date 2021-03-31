type t =
  | Bind of string * value_type
  | Move of string * value_type
  | Call of label_type * value_type list
  | Test of value_type * (value_type * value_type list) * (value_type * value_type list)
  | Value of value_type
  | Return
  | Allocate of value_type list
  | AccessClosure of string * int list
and value_type =
  | Int of int
  | Bool of bool
  | Primitive of string
  | Label of int
  | Box of int
  | Ref of string
  | Nil
  | Quote of AstType.t
  | Cons of value_type * value_type
