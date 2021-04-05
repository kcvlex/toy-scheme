open ThreeAddressCodeType

(* val to_abs_program : ThreeAddressCodeType.t -> AbstractMachineType.t *)

val from_abs_program : AbstractMachineType.t -> t

val get_instr_id : instr_type -> int

val get_function : t -> string -> instr_type list

val sample_program : t
