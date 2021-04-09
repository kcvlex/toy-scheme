type t = (string, proc_type) Hashtbl.t
and proc_type = clsr_arg_type * common_arg_type * extend_arg_type * instr_type list
and clsr_arg_type = string option
and common_arg_type = string list
and extend_arg_type = string option
and instr_type =
  | Bind of string * value_type
  | Test of value_type * instr_type list * instr_type list
  | Return of value_type
  | Jump of string
  | Call of value_type * value_type list
and value_type =
  | Int of int
  | Bool of bool
  | Primitive of SymbolType.primitive_sym
  | PrimCall of SymbolType.primitive_sym * value_type list
  | Label of string
  | Ref of string
  | Allocate of value_type list
  | RA
  | Nil
  | Quote of AstType.t
  | Cons of value_type * value_type
  | AccessClosure of string * int list
