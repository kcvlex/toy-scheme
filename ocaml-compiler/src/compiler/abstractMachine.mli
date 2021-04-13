type t

val translate : ClosureType.t -> t

val eval : t -> unit

val link : AbstractMachineType.t -> AbstractMachineType.t -> AbstractMachineType.t

val add_glob : t -> string -> AbstractMachineType.data_type option -> unit

val make_machine : AbstractMachineType.t -> t

val call_and_print : string -> 
                     string -> 
                     AbstractMachineType.value_type list -> 
                     AbstractMachineType.t

val string_of_program : AbstractMachineType.t -> string

val string_of_machine : t -> string
