type t
(*@ model contents : int*)

val init_sut : unit -> t
(*@ 
tval = init_sut ()
ensures tval.contents = 5*)

val test : t -> int
(*@*)


(*the error here is garbage *)

