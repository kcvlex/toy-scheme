open RegsType

val make_reg_set : (int * int * int) -> reg_set

val string_of_reg : t -> string

val string_of_regtbl : reg_table -> string
