open SymbolType

type t = SymbolType.t

let string_of_rsym rsym = match rsym with
  | Record i -> "__r" ^ (string_of_int i)

let string_of_csym csym = match csym with
  | Cont i -> "__k" ^ (string_of_int i)

let string_of_psym psym = match psym with
  | Proc i -> "__f" ^ (string_of_int i)

let string_of_clsym clsym = match clsym with
  | Closure i -> "__c" ^ (string_of_int i)

let string_of_sym sym = match sym with
  | RecordSym r -> string_of_rsym r
  | ContSym c -> string_of_csym c
  | ClosureSym c -> string_of_clsym c
  | Primitive s -> s
  | CommonSym s -> s
  | ParamSym i -> "__t" ^ (string_of_int i)
  | ProcSym p -> string_of_psym p

let to_pair sym = match sym with
  | CommonSym s -> (0, s)
  | Primitive s -> (1, s)
  | ContSym (Cont n) -> (2, string_of_int n)
  | ParamSym n -> (3, string_of_int n)
  | RecordSym (Record n) -> (4, string_of_int n)
  | ClosureSym (Closure n) -> (5, string_of_int n)
  | ProcSym (Proc n) -> (6, string_of_int n)

let compare t1 t2 =
  let t1 = to_pair t1 in
  let t2 = to_pair t2 in
  match (t1, t2) with
    | ((a, b), (c, d)) -> if a != c then Int.compare a c else String.compare b d
