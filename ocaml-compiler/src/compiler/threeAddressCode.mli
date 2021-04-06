open ThreeAddressCodeType

(* val to_abs_program : ThreeAddressCodeType.t -> AbstractMachineType.t *)

val from_abs_program : AbstractMachineType.t -> t

val get_instr_id : instr_type -> int

val replace_id : instr_type -> int -> instr_type

val string_of_instr : instr_type -> string

val sample_program : t
