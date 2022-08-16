(*small test for old operator and if statements in equations*)

type t
(*@ model contents: integer *)


val compare_and_set: t -> int -> int -> bool
(*@
out = compare_and_set r seen v
modifies r.contents
ensures r.contents = (if ((old r.contents) = seen) 
then v else old r.contents)
*)
