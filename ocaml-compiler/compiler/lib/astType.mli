type t =
  | Num of int
  | Bool of bool
  | Symbol of string
  | Lambda of string list * t
  | Apply of t * t list
  | Define of bind list * t
  | Branch of t * t * t
and bind = Bind of { sym : string; def : t; }
