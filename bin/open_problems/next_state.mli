type t
(*@ model field1 : integer
model field2 : integer
*)

val init_sut : unit -> t
(*@ 
out = init_sut ()
ensures out.field1 = 5
ensures out.field2 = 6
*)


val test : t -> int
(*@ out = test arg 
(*pure*)
ensures arg.field1 = arg.field2
ensures arg.field2 = arg.field1
 *)
