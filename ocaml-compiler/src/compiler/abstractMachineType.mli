type t = func_table * jump_table
and func_table = (string, proc_type) Hashtbl.t
and jump_table = (string, instr_type list) Hashtbl.t
and proc_type = clsr_arg_type * common_arg_type * extend_arg_type * instr_type list
and clsr_arg_type = ref_type option
and common_arg_type = ref_type list
and extend_arg_type = ref_type option
and instr_type =
  | Bind of ref_type * value_type
  | Test of value_type * instr_type list * instr_type list
  | Return of value_type
  | Jump of value_type
  | Call of value_type * value_type list
and value_type =
  | Int of int
  | Bool of bool
  | Nil
  | Primitive of SymbolType.primitive_sym
  | PrimCall of SymbolType.primitive_sym * value_type list
  | Label of string
  | Ref of ref_type
  | Allocate of value_type list
  | Quote of AstType.t
  | Cons of value_type * value_type
  | AccessClosure of string * int list
and ref_type =
  | Global of string
  | Local of string
  | RV
and data_type =
  | DInt of int
  | DBool of bool
  | DLabel of string
  | DNil
  | DCons of data_type * data_type
  | DList of data_type list
  | DPrim of SymbolType.primitive_sym
  | DQuote of AstType.t
