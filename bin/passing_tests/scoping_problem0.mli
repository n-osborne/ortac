type t
(*@ model contents : integer*)

val init_sut : unit -> t
(*@ 
out = init_sut ()
ensures out.contents = 5*)


val test : t -> int
(*@ out = test arg 
(*pure*)
ensures arg.contents = out
 *)



(*if you wrote
val get : t -> int -> int
    out = get s i
    s.field1 = out
    this would break the next_state function generation because out is not in scope
    need to check the rhs of all the next states does not use the return name.
    this should go in the postcondition, not in next state. but how to know that?

    ^ no way to get around writing the contains_ident function.
    can i just run the ocaml typechecker on the = expression with out outside of scope?
    how does the gospel typechecker check scoping <- it's a mess
  *)

