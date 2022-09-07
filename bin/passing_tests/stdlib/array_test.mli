type t
(*@ model state : integer list*)

val init_sut : unit -> t
(*@ out = init_sut ()
ensures out.state = List.init 16 (fun _x -> 0)
*)

val length : t -> int
(*@
out = length a
pure
ensures out = (List.length a.state)
*)

val get : t -> int -> int
(*@
out = get a n
checks 0 <= n /\ n < (List.length a.state)
ensures out = List.nth a.state n
*)

val set : t -> int -> int -> unit
(*@ () = set a n c
modifies a.state
checks (0 <= n) /\ (n < List.length a.state)
ensures a.state = List.mapi
(fun i b -> if (i = (integer_of_int n)) then (integer_of_int c) else b)
(old a.state)
 *)

val copy: t -> t
(*@
out = copy a
pure
ensures out.state = a.state
*)

val fill: t -> int -> int -> int -> unit
(*@ () = fill a pos len c
modifies a.state
checks pos >= 0 && len >= 0 && pos <= List.length a.state
&& len <= List.length a.state &&
pos + len <= List.length a.state
ensures a.state = List.mapi
(fun i -> fun v -> 
        if ((integer_of_int pos) <= i && 
        i < (integer_of_int pos) + (integer_of_int len)) 
        then (integer_of_int c) 
        else v) (old a.state)
*)

val to_list: t -> int list
(*@
out = to_list a
pure
ensures List.map (fun i  -> integer_of_int i) out = a.state
*)

val mem: int -> t -> bool
(*@
b = mem c a 
pure
ensures b = List.mem (integer_of_int c) a.state
*)




val sub : t -> int -> int -> t
(*@
a1 = sub a pos len
checks pos >= 0 && len >= 0 && pos <= List.length a.state
&& len <= List.length a.state &&
pos + len <= List.length a.state
ensures a1.state = List.fold_right 
(fun tup acc -> 
if (pos <= (fst tup) && (fst tup) <= pos + len - 1)
then (snd tup)::acc
else acc
) 
(List.mapi (fun i v -> (i, v))  a.state) []
*)


val sort : t -> unit
(*@
() = sort a
modifies a.state
ensures a.state = 
List.fold_right 
(fun ele sorted -> 
let (left, right) = (List.fold_right 
                      (fun x out -> if x <= ele then 
                              x::out else out) sorted [],
                     List.fold_right 
                      (fun x out -> if x > ele then 
                              x::out else out) sorted []) in 
List.fold_right (fun x out -> x::out) left (ele::right)) 
(old a.state) 
[]  
*)

(*can't do to_seq because of the unconventional run function*)
