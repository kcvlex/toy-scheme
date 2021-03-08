open SymbolType

let string_of_rsym rsym = match rsym with
  | Record i -> "__c" ^ (string_of_int i)

let string_of_csym csym = match csym with
  | Cont i -> "__k" ^ (string_of_int i)

let string_of_psym psym = match psym with
  | Proc i -> "__f" ^ (string_of_int i)

let string_of_sym sym = match sym with
  | RecordSym r -> string_of_rsym r
  | ContSym c -> string_of_csym c
  | Primitive "+" -> "add"
  | Primitive "-" -> "sub"
  | Primitive "<" -> "less"
  | Primitive s -> s
  | CommonSym s -> s
  | ParamSym i -> "__t" ^ (string_of_int i)
