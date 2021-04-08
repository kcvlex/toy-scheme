type 'a t

val make : bool -> 'a t

val add_node : 'a t -> 'a -> unit

val add_edge : 'a t -> 'a -> 'a -> unit

val rm_node : 'a t -> 'a -> unit

val rm_edge : 'a t -> 'a -> 'a -> unit

val contraction : 'a t -> 'a -> 'a -> unit

val represent : 'a t -> 'a -> 'a

val group : 'a t -> 'a -> 'a list

(* representative of each group *)
val nodes : 'a t -> 'a list

val succ : 'a t -> 'a -> 'a list

val pred : 'a t -> 'a -> 'a list

val has_edge : 'a t -> 'a -> 'a -> bool

val degree : 'a t -> 'a -> int

val length : 'a t -> int

val is_same_group : 'a t -> 'a -> 'a -> bool

val dump : 'a t -> ('a -> string) -> string
