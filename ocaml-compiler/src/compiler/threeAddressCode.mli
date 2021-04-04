(* val to_abs_program : ThreeAddressCodeType.t -> AbstractMachineType.t *)

val from_abs_program : AbstractMachineType.t -> ThreeAddressCodeType.t

val get_instr_id : ThreeAddressCodeType.instr_type -> int

val sample_program : ThreeAddressCodeType.t
