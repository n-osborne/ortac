type t
(*@ model state : (int * int) list*)

val init_sut : unit -> t
(*@ h = init_sut ()
ensures h.state = []
*)

val clear : t -> unit
(*@ () = clear h
 modifies h.state 
 ensures h.state = []
  *)

val add : t -> int -> int -> unit
(*@ () = add h key data 
  modifies h.state 
  ensures h.state = (key, data)::(old h.state)
  *)

val remove: t -> int -> unit
(*@ () = remove h key
 modifies h.state
 ensures h.state = List.rev 
 (snd (List.fold_left (fun acc pair ->
         let found = fst acc in
        let out = snd acc in
        let k = fst pair in
        let d = snd pair in 
         if found then (found, pair::out) else
                 if key = k then (true, out)
                            else (false, pair::out))
 (false, []) (old h.state) ))
 *)

val find: t -> int -> int
(*@
 data = find h key 
 raises Not_found -> not (List._exists (fun pair -> fst pair = key) h.state)
ensures Some data = List.fold_left (fun acc pair -> 
        match acc with
      | None -> if (fst pair) = key 
        then Some (snd pair) else None
      | Some _ -> acc) None h.state 
  *)

val find_opt: t -> int -> int option
(*@
 data_opt = find_opt h key
pure 
ensures data_opt = List.fold_left (fun acc pair -> 
        match acc with
      | None -> if (fst pair) = key then Some (snd pair) else None
      | Some _ -> acc) None h.state
  *)  

val find_all: t -> int -> int list
(*@
 data = find_all h key 
 pure
 ensures data = List.fold_right (fun pair acc -> 
      if (fst pair) = key then (snd pair)::acc
      else acc) h.state []
  *)

val replace:  t -> int -> int -> unit
(*@ () = replace h key data 
  modifies h.state
 ensures h.state = (key, data):: (List.rev 
 (snd (List.fold_left (fun acc pair ->
         let found = fst acc in
        let out = snd acc in
        let k = fst pair in
        let d = snd pair in 
         if found then (found, pair::out) else
                 if key = k then (true, out)
                            else (false, pair::out))
 (false, []) (old h.state) )))

  *)

val mem : t -> int  -> bool
(*@
 out = mem h key
 pure
 ensures out = List._exists (fun pair -> fst pair = key) h.state
 *)

val length : t -> int
(*@
 out = length h
 pure
 ensures out = List.length (h.state)
 *)
