type t

val trans : AstType.t -> t

val flatten : t -> InterlangType.t

val compile : AstType.t -> InterlangType.t
