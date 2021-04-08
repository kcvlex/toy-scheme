open Util
open ThreeAddressCodeType

type t

val analyze : reg_set ->
              labeled_instr Vector.t ->
              label_table -> t

val live_in : t -> int -> reg_table

val live_out : t -> int -> reg_table

val get_use : t -> int -> reg_table

val get_def : t -> int -> reg_table

val length : t -> int

val dump : t -> string
