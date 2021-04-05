type ('a, 'b) t

val make : bool -> ('a, 'b) t

val set_node : ('a, 'b) t -> 'a -> 'b -> unit

val add_edge : ('a, 'b) t -> 'a -> 'a -> unit

let rm_node : ('a, 'b) t -> 'a -> unit

val rm_edge : ('a, 'b) t -> 'a -> 'a -> unit

val get_nodes : ('a, 'b) t -> 'a list

val succ : ('a, 'b) t -> 'a -> 'a list

val pred : ('a, 'b) t -> 'a -> 'a list

val get : ('a, 'b) t -> 'a -> 'b

val has_edge : ('a, 'b) t -> 'a -> 'a -> bool

val degree : ('a, 'b) t -> 'a -> int

val length : ('a, 'b) t -> int

val dump : ('a, 'b) t -> ('a -> string) -> string
