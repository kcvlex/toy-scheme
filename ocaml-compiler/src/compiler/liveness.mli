open Util
open ThreeAddressCodeType

type t

val analyze : instr_type list -> (string, int) Hashtbl.t -> t

val live_in : t -> int -> reg_set

val live_out : t -> int -> reg_set

val get_use : t -> int -> reg_set

val get_def : t -> int -> reg_set

val length : t -> int
