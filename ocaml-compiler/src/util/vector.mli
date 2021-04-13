type 'a t

val length : 'a t -> int

val push_back : 'a t -> 'a -> unit

val get : 'a t -> int -> 'a

val rget : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val make : int -> 'a -> 'a t

val empty : unit -> 'a t

val pop_back : 'a t -> unit

val pops : 'a t -> int -> unit

val copy : 'a t -> 'a t

val to_list : 'a t -> 'a list

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val list_of_vector : 'a t -> 'a list

val vector_of_list : 'a list -> 'a t
