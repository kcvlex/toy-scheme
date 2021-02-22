type t =
  | Num of int
  | Bool of bool
  | Symbol of string
  | Lambda of t list * t
  | LambdaVar of t * t
  | Apply of t * t list
  | Define of t * t
  | Cons of t * t
