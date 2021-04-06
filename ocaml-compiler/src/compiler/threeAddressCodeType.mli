open Util

type t = (string, arguments_type) Hashtbl.t * (string, int) Hashtbl.t * instr_type Vector.t
and instr_type =
  | Move of reg_type * value_type * int
  | Test of value_type * value_type * int
  | Jump of value_type * int  (* FIXME : rename to call *)
  | Return of int
  | Load of reg_type * value_type * int * int   (* Load virtual register to actual register *)
  | Store of reg_type * value_type * int * int (* Store actual register related to virtual register *)
  | PrimCall of reg_type * SymbolType.primitive_sym * value_type list * int
and value_type =
  | Int of int
  | Bool of bool
  | Reg of reg_type
  | Nil
  | FuncLabel of string
  | JumpLabel of string
  | Primitive of SymbolType.primitive_sym
  | Quote of AstType.t
and reg_type =
  | RV
  | CallerSave of int
  | CalleeSave of int
  | Argument of int
  | Virtual of int
and arguments_type = clsr_arg_type * common_arg_type * extend_arg_type
and clsr_arg_type = string option
and common_arg_type = string list
and extend_arg_type = string option
and reg_set = (reg_type, unit) Hashtbl.t
