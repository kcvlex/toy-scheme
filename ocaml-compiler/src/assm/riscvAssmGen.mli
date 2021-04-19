open Compiler
open RiscvAssm

val generate : ThreeAddressCodeType.t -> t

val string_of_pseudo : pseudoinstr_type -> string

val string_of_actual : actualinstr_type -> string

val string_of_instr : assm_instr_type -> string

val string_of_ops : Rv32i.ops_type -> string

val string_of_assm : t -> string
