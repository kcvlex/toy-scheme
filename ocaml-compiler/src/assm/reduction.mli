open RiscvAssm

(* Reduce assm code *)
val reduction : t -> t

(* Convert pseudo instructions to actual instructions *)
val convert : t -> t
