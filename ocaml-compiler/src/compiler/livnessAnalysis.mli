open Util

type t

val analysis : ThreeAddressCodeType.t -> t

val live_in : t -> int -> ThreeAddressCodeType.reg_set

val live_out : t -> int -> ThreeAddressCodeType.reg_set

val length : t -> int
