open Util

type reg_type = RegsType.t

type t = {
  signature : (string, arguments_type) Hashtbl.t;
  ltbl : (string, int) Hashtbl.t;
  seq : labeled_instr Vector.t;
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
  | Primitive of SymbolType.primitive_sym
  | FuncLabel of string
  | JumpLabel of string
  | Quote of AstType.t
and arguments_type = clsr_arg_type * common_arg_type * extend_arg_type
and clsr_arg_type = string option
and common_arg_type = string list
and extend_arg_type = string option
and labeled_instr = instr_type * string option
and label_table = (string, int) Hashtbl.t
