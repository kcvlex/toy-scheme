type ('a, 'b) t

val make : bool -> ('a, 'b) t

val add_node : ('a, 'b) t -> 'a -> 'b -> unit

val add_edge : ('a, 'b) t -> 'a -> 'a -> unit

val rm_edge : ('a, 'b) t -> 'a -> 'a -> unit

val succ : ('a, 'b) t -> 'a -> 'a list

val pred : ('a, 'b) t -> 'a -> 'a list

val get : ('a, 'b) t -> 'a -> 'b

val length : ('a, 'b) t -> int
