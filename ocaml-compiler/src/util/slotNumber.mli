type 'a t

val make : (int -> 'a) -> 'a t

val fresh : 'a t -> 'a

val fresh_list : 'a t -> int -> 'a list
