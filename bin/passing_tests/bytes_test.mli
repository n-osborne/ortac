
type t
(*@ model contents : char list*)

val init_sut: unit -> t
(*@ 
bytes = init_sut ()
ensures bytes.contents = List.init 42 (fun _x -> '0') 
*)

(*this is a hack but its just until gospel supports strings and machine ints.
i could include more fns here and it would decrease the amount of integer_of_ints
necessary below
 *)
module String: sig
val init : int -> (int -> char) -> string 
(*@ pure *) (*cant figure out how to specify this because of the fn argument
not taking an integer *)

val get : string -> int -> char
(*@ pure *) 

val length : string -> int
(*@ pure *)

end

module Int: sig
val add : int -> int -> int
(*@ pure *)
val sub :  int -> int -> int 
(*@ pure *)
end


val length : t -> int 
(*@
out = length bytes 
pure
ensures out = List.length bytes.contents
*)

val get : t -> int -> char 
(*@ out = get bytes n
checks (0 <= n) /\ (n < List.length bytes.contents)
ensures out = List.nth bytes.contents n
*)

val set : t -> int -> char -> unit
(*@ () = set bytes n c
modifies bytes.contents
checks (0 <= n) /\ (n < List.length bytes.contents)
ensures bytes.contents = List.mapi 
(fun i b -> if (i = (integer_of_int n)) then c else b)
(old bytes.contents)
 *)

val fill: t -> int -> int -> char -> unit
(*@
() = fill bytes pos len c 
modifies bytes.contents
checks pos >= 0 && len >= 0 && pos <= List.length bytes.contents
&& len <= List.length bytes.contents &&
pos + len <= List.length bytes.contents
ensures bytes.contents = List.mapi 
(fun i -> fun v -> if ((integer_of_int pos) <= i && i < (integer_of_int pos) + (integer_of_int len)) then c else v) (old bytes.contents)
*)


val sub_string: t -> int -> int -> string 
(*@
out = sub_string bytes pos len 
checks pos >= 0 && len >= 0 && pos <= List.length bytes.contents 
&& len <= List.length bytes.contents &&  
pos + len <= List.length bytes.contents
ensures out = String.init len 
(fun i -> List.nth bytes.contents ((integer_of_int i) + (integer_of_int pos)))
*)


val index_from: t -> int -> char -> int 
(*@
out = index_from s i c
checks i >= 0 /\ i <= List.length s.contents
raises Not_found -> List.for_all (fun tup -> (fst tup) < i || c <> (snd tup)) 
(List.mapi (fun i c -> (i, c)) s.contents)
ensures (List.nth s.contents out) = c /\ 
out >= i /\ 
List.for_all (fun tup -> (fst tup) < i || c <> (snd tup) || out <= (fst tup))
(List.mapi (fun i c -> (i, c)) s.contents)
*)

module List: sig
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
(*@ pure *)

val length : 'a list -> int
(*@ pure*)
end

val blit_string : string -> int -> t -> int -> int -> unit
(*@
() = blit_string src src_pos dst dst_pos len
checks src_pos >= 0 && len >= 0 && src_pos <= String.length src &&
len <= String.length src && src_pos + len <= String.length src
checks dst_pos >= 0 && len >= 0 && dst_pos <= List.length dst.contents 
&& len <= List.length dst.contents &&
dst_pos + len <= (List.length dst.contents)
modifies dst
ensures dst.contents = List.mapi 
(fun i  -> fun v -> if ((integer_of_int dst_pos) <= (integer_of_int i) && 
	(integer_of_int i) < (integer_of_int dst_pos) + (integer_of_int len))
        then let offset = Int.sub i dst_pos in 
	String.get src (Int.add src_pos offset)
        else v) (old dst.contents)
*)
