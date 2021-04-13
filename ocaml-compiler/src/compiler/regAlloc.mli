open Util

type t

(* val allocate : ThreeAddressCodeType.t -> (int * int * int) -> ThreeAddressCodeType.t *)

val allocate_experiment : (int * int * int) -> 
                          ThreeAddressCodeType.t ->
                          ThreeAddressCodeType.t
