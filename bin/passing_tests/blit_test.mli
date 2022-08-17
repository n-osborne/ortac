type t
(*@ model contents : char list*)


val init_sut: unit -> t
(*@
bytes = init_sut ()
ensures bytes.contents = List.init 42 (fun _x -> '0')
*)

val blit_string : string -> int -> t -> int -> int -> int
(*@ out = blit_string src src_pos dst dst_pos len
pure
*)
