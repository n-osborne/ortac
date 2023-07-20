(* $MDX part-begin=f *)
val f : int -> int
(*@ y = f x *)
(* $MDX part-end *)

(* $MDX part-begin=g *)
val g : int -> bool
(*@ b = f x *)
(* $MDX part-end *)
