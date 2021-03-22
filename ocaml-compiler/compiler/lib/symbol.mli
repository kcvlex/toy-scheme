open SymbolType

type t = SymbolType.t

val string_of_rsym : record_sym -> string

val string_of_csym : cont_sym -> string

val string_of_clsym : closure_sym -> string

val string_of_psym : proc_sym -> string

val string_of_sym : t -> string

val compare : t -> t -> int
