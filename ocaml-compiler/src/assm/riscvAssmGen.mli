open Compiler
open RiscvAssm

val generate : ThreeAddressCodeType.t -> t

val string_of_imm : Rv32i.imm_type -> string

val string_of_reg : Rv32i.reg_type -> string

val string_of_pseudo : PseudoInstr.t -> string

val string_of_actual : ActualInstr.t -> string

val string_of_instr : assm_instr_type -> string

val string_of_ops : Rv32i.ops_type -> string

val string_of_assm : t -> string
