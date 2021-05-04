open Compiler
open Riscv
open RiscvAssm

val generate : arch_type -> build_type -> ThreeAddressCodeType.t -> t

val link_builtin : t -> t

val string_of_imm : Riscv.imm_type -> string

val string_of_reg : Riscv.reg_type -> string

val string_of_pseudo : PseudoInstr.t -> string

val string_of_actual : ActualInstr.t -> string

val string_of_instr : assm_instr_type -> string

val string_of_ops : Riscv.ops_type -> string

val string_of_assm : t -> string
