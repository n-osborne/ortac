(*simple test to see if my tool translates machine ints to machine ints *)

type t
(*@ model contents: int *)

val init_sut: unit -> t
(*@ ret = init_sut ()
 ensures ret.contents = 42 *)

val get : t -> int
(*@
v = get r
pure*)
