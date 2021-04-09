open ThreeAddressCodeType
open Util

val to_abs_program : ThreeAddressCodeType.t -> AbstractMachineType.t

val from_abs_program : AbstractMachineType.t -> t

val get_instr_id : instr_type -> int

val replace_id : instr_type -> int -> instr_type

val split_program : ThreeAddressCodeType.t -> (string Vector.t * labeled_instr Vector.t)

val string_of_instr : instr_type -> string

val string_of_reg : reg_type -> string

val string_of_regtbl : reg_table -> string

val make_reg_set : (int * int * int) -> reg_set

val sample_program : t
