type t
(*@ model contents : integer*)

val init_sut : unit -> t
(*@ 
out = init_sut ()
ensures out.contents = 5*)

val init_sut : unit -> t
(*@
out = init_sut ()
ensures out.contents = 4*)

val test : t -> int
(*@ pure *)
