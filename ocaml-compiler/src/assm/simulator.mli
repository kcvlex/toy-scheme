(*
type t

val init : (int * int * int) -> RiscvAssm.t -> t

val eval : t -> unit

val step : t -> bool

val dump_regfile : t -> string
*)
