type t
(*@ model contents : integer*)

val init_sut : unit -> t
(*@ 
tval = init_sut ()
ensures tval.contents = 5*)

val test : t -> int
(*@ out = test tval 
ensures tval.contents = 6*)

val test1 : t -> int
(*@ out = test1 tval 
ensures tval.contents = 6*)
