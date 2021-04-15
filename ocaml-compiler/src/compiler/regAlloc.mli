open Util

type t

val allocate : (int * int * int) -> 
               ThreeAddressCodeType.t ->
               ThreeAddressCodeType.t

val allocate_experiment : (int * int * int) -> 
                          ThreeAddressCodeType.t ->
                          ThreeAddressCodeType.t
