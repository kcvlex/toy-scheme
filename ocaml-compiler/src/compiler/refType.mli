type t =
  | Direct of SymbolType.t
  | PassedClosure of SymbolType.closure_sym * int
  | LocalBind of SymbolType.t * SymbolType.record_sym * int
  | PassingRecord of SymbolType.t * SymbolType.record_sym * int
