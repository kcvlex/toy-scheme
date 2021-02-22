type t

val dprog2cps : t -> Cps.t

val dexp2cps : t -> Cps.cps_cont option -> Cps.t

val dtriv2cps : t -> Cps.t

val dcons2cps : t -> Cps.cps_cont option -> Cps.t

val convert_ast : Ast.t -> t

val cps_transformation : Ast.t -> Cps.t
