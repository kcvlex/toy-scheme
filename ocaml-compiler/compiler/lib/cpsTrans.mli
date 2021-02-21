val dprog_to_cps : Ast.t -> Cps.t

val dexp_to_cps : Ast.t -> Cps.cps_cont option -> Cps.t

val dtriv_to_cps : Ast.t -> Cps.t

val cps_transformation : Ast.t -> Cps.t
