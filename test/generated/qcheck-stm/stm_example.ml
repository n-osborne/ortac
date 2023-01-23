type t = int ref

let init_sut () = ref 0

let add_sut r i =
  r := !r + i;
  !r
