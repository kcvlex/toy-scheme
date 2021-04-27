open RiscvAssm
open Compiler

val add : t

val sub : t

val eq : t

val less : t

val null : t

val cons : t

val car : t

val cdr : t

val list_ref : t

val apply : t

val display : t

val allocate : t

val func_of_prim : SymbolType.primitive_sym -> string

val closure_of_prim : SymbolType.primitive_sym -> string

val lib : t
