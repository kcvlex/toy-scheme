type t

val empty : t

val make : passed : SymbolType.closure_sym * SymbolType.t list ->
           created : SymbolType.record_sym * SymbolType.t list ->
           defined : SymbolType.t list -> t

val reftype : t -> SymbolType.t -> RefType.t
