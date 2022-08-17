
type t

val init_sut: unit -> t
(*@
bytes = init_sut ()
*)

module List: sig
val length : 'a list -> int
(*@ pure*)
end

(*how to make this go through the driver?*)
val test : t -> int list
(*@ out = test arg 
ensures (List.length out = 0)
*)

