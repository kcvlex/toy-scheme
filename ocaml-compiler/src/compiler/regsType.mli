type t = 
  | CallerSaved of int
  | CalleeSaved of int
  | Argument of int
  | Virtual of int
and reg_set = {
  caller_saved_regs : t list;
  callee_saved_regs : t list;
  argument_regs : t list;
  all_regs : t list;
  reg_sum : int;
}
and reg_table = (t, unit) Hashtbl.t
