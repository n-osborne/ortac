type t
(*@ model state : integer *)

val init_sut : unit -> t
(*@ t = init_sut ()
    ensures t.state = 0 *)

val add_sut : t -> int -> int
(*@ res = add_sut t i

    modifies t

    ensures t.state = res                               (* proscriptive ensure => postcond   *)
    ensures t.state - res = 0                           (* proscriptive ensure => postcond   *)

    ensures t.state >= 0 /\ t.state = (old t.state) + i (* descriptive ensure =/> next_state *)
    ensures t.state = (old t.state) + i                 (* descriptive ensure => next_state  *)
    ensures t.state - (old t.state) = i                 (* descriptive ensure =/> next_state *)
    ensures t.state - i = (old t.state)                 (* descriptive ensure =/> next_state *)
*)
