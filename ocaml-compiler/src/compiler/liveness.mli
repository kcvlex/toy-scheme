open Util
open ThreeAddressCodeType

type t

val analyze : RegsType.reg_set ->
              labeled_instr Vector.t ->
              label_table ->
              (reg_type -> bool) ->
              t

val live_in : t -> int -> RegsType.reg_table

val live_out : t -> int -> RegsType.reg_table

val get_use : t -> int -> RegsType.reg_table

val get_def : t -> int -> RegsType.reg_table

val length : t -> int

val dump : t -> string
