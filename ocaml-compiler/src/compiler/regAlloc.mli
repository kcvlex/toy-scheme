open Util

type t

(* val allocate : ThreeAddressCodeType.t -> (int * int * int) -> ThreeAddressCodeType.t *)

val allocate_experiment : (int * int * int) -> 
                          ThreeAddressCodeType.instr_type Vector.t ->
                          (string, int) Hashtbl.t ->
                          (ThreeAddressCodeType.instr_type Vector.t * (string, int) Hashtbl.t)
