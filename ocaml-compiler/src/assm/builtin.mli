open RiscvAssm
open Compiler

val func_of_prim : SymbolType.primitive_sym -> string

val closure_of_prim : SymbolType.primitive_sym -> string

val lib : context_type -> t
