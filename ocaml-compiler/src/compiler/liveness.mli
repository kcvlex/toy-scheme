open Util

type t

val analyze : ThreeAddressCodeType.t -> t

val live_in : t -> int -> ThreeAddressCodeType.reg_set

val live_out : t -> int -> ThreeAddressCodeType.reg_set

val get_use : t -> int -> ThreeAddressCodeType.reg_set

val get_def : t -> int -> ThreeAddressCodeType.reg_set

val length : t -> int
