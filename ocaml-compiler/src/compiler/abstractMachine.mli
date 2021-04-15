type t

val translate : ClosureType.t -> AbstractMachineType.t

val eval : t -> unit

val link : AbstractMachineType.t -> AbstractMachineType.t -> AbstractMachineType.t

val make_machine : AbstractMachineType.t -> t

val call_and_print : string -> 
                     string -> 
                     AbstractMachineType.value_type list -> 
                     AbstractMachineType.t

val raw_allocate : int -> AbstractMachineType.t

val string_of_ref : AbstractMachineType.ref_type -> string

val string_of_program : AbstractMachineType.t -> string

val string_of_machine : t -> string
