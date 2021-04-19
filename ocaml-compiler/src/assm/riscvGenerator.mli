open Compiler

val generate : ThreeAddressCodeType.t -> RiscvInstr.t list * int

val string_of_instr : RiscvInstr.t -> string
