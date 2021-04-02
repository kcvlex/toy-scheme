type t

val translate : ClosureType.t -> t

val eval : t -> unit

val string_of_abs : AbstractMachineType.t -> string

val string_of_program : t -> string
