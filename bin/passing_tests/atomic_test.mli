type t
(*@ model contents: int *)

val init_sut: unit -> t
(*@ ret = init_sut ()
 ensures ret.contents = 42 *)


val get : t -> int
(*@
v = get r
pure
ensures v = r.contents
*)

val set : t -> int -> unit 
(*@ () = set r s
modifies r.contents
ensures r.contents = s
*)

val exchange : t -> int -> int
(*@ out = exchange r i
modifies r.contents
ensures r.contents = i
ensures out = old r.contents
*)

val fetch_and_add : t -> int -> int
(*@ out = fetch_and_add r n 
modifies r.contents
ensures r.contents = (old r.contents) + n
ensures out = old r.contents
*) 

val compare_and_set: t -> int -> int -> bool
(*@
out = compare_and_set r seen v
modifies r.contents
ensures r.contents = (if ((old r.contents) = seen) 
then v else old r.contents)
ensures out <-> ((old r.contents) = seen)
*)

val incr : t -> unit
(*@
() = incr r
modifies r.contents
ensures r.contents = (old r.contents) + 1
*)

val decr : t -> unit
(*@
() = decr r
modifies r.contents
ensures r.contents = (old r.contents) - 1
*)
