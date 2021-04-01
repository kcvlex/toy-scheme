open SymbolType

type t = SymbolType.t

let string_of_prim p = match p with
  | ADD     -> "+"
  | SUB     -> "-"
  | MUL     -> "*"
  | DIV     -> "/"
  | EQ      -> "eq?"
  | NULL    -> "null?"
  | LESS    -> "<"
  | CONS    -> "cons"
  | CAR     -> "car"
  | CDR     -> "cdr"
  | LIST    -> "list"
  | LISTREF -> "list-ref"
  | APPLY   -> "apply"
  | MAP     -> "map"
  | DISPLAY -> "display"

let string_of_sym sym = match sym with
  | ContSym i -> "__k" ^ (string_of_int i)
  | ParamSym i -> "__t" ^ (string_of_int i)
  | PrimitiveSym p -> string_of_prim p
  | CommonSym s -> s

let to_pair sym = match sym with
  | CommonSym s -> (0, s)
  | PrimitiveSym p -> (1, string_of_prim p)
  | ContSym n -> (2, string_of_int n)
  | ParamSym n -> (3, string_of_int n)

let compare t1 t2 =
  let t1 = to_pair t1 in
  let t2 = to_pair t2 in
  match (t1, t2) with
    | ((a, b), (c, d)) -> if a != c then Int.compare a c else String.compare b d
