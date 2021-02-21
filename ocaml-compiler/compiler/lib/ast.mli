type t =
  | Num of int
  | Bool of bool
  | Symbol of string
  | Lambda of string list * t
  | Apply of t * t list
  | Define of string * t
  | Cons of t * t
