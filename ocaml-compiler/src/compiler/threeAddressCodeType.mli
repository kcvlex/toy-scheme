open Util

type t = {
  signature : (string, arguments_type) Hashtbl.t;
  label_tbl : (string, int) Hashtbl.t;
  seq : (instr_type * string option) Vector.t;
}
and instr_type =
  | Bind of reg_type * value_type * int
  | Move of reg_type * reg_type * int
  | Test of value_type * value_type * int
  | Jump of value_type * int  (* FIXME : rename to call *)
  | Return of int
  | Load of reg_type * value_type * int * int   (* Load virtual register to actual register *)
  | Store of reg_type * value_type * int * int (* Store actual register related to virtual register *)
and value_type =
  | Int of int
  | Bool of bool
  | Reg of reg_type
  | Nil
  | PrimCall of SymbolType.primitive_sym * value_type list
  | FuncLabel of string
  | JumpLabel of string
  | Primitive of SymbolType.primitive_sym
  | Quote of AstType.t
and reg_type =
  | RV
  | CallerSaved of int
  | CalleeSaved of int
  | Argument of int
  | Virtual of int
  | RA
and arguments_type = clsr_arg_type * common_arg_type * extend_arg_type
and clsr_arg_type = string option
and common_arg_type = string list
and extend_arg_type = string option
and reg_table = (reg_type, unit) Hashtbl.t
and labeled_instr = instr_type * string option
and label_table = (string, int) Hashtbl.t
and reg_set = {
  caller_saved_regs : reg_type list;
  callee_saved_regs : reg_type list;
  argument_regs : reg_type list;
  all_regs : reg_type list;
  reg_sum : int;
}
