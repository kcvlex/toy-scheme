type t

val translate : ClosureType.t -> t

val eval : t -> unit

val string_of_program : AbstractMachineType.t -> string
