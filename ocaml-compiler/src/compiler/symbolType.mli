type t =
  | ContSym of int
  | ParamSym of int
  | PrimitiveSym of primitive_sym
  | CommonSym of string
and primitive_sym =
  | ADD | SUB | MUL | DIV 
  | EQ | LESS | NULL
  | CONS | CAR | CDR | LIST | LISTREF
  | APPLY | MAP 
  | DISPLAY
