type t =
  | Num of int
  | Bool of bool
  | Symbol of string
  | Primitive of string
  | Lambda of string list * string option * t
  | Apply of t * t list
  | Define of bind list * t
  | Let of bind list * t
  | Branch of t * t * t
  | Statement of t list
and bind = string * t 
