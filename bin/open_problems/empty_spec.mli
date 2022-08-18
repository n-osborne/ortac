type t
(*@ model contents : integer*)

val init_sut : unit -> t
(*@ 
out = init_sut ()
ensures out.contents = 5*)


val test : t -> int
(*@ *)
