type t
(*@ model contents: integer *)

val init_sut: unit -> t
(*@ ret = init_sut ()
 ensures ret.contents = 42 *)


val get : t -> int
(*@
v = get atom
pure
ensures v = atom.contents
*)

val set : t -> int -> unit 
(*@ () = set atom s
modifies atom.contents
ensures atom.contents = s
ensures (fun x -> true) 5 = true 
*)

val exchange : t -> int -> int
(*@ out = exchange atom i
modifies atom.contents
ensures atom.contents = i
*) 




