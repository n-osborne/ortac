
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* NOTE:
   If this file is arrayLabels.mli, run tools/sync_stdlib_docs after editing it
   to generate array.mli.

   If this file is array.mli, do not edit it directly -- edit
   arrayLabels.mli instead.
 *)

(** Array operations.

    The labeled version of this module can be used as described in the
    {!StdLabels} module.
*)

type 'a t (* = 'a array *)
(** An alias for the type of arrays. *)
(*@ model length : int
    mutable model contents : 'a seq
*)

external length : 'a t -> int = "%array_length"
(** Return the length (number of elements) of the given array. *)
(*@ i = length arr
    ensures i = arr.length *)

external get : 'a t -> int -> 'a = "%array_safe_get"
(** [get a n] returns the element number [n] of array [a].
   The first element has number 0.
   The last element has number [length a - 1].
   You can also write [a.(n)] instead of [get a n].

   @raise Invalid_argument
   if [n] is outside the range 0 to [(length a - 1)]. *)
(*@ a = get arr n
    raises Invalid_argument _ -> n < 0 \/ n >= arr.length
    ensures a = Seq.get arr.contents n *)

external set : 'a t -> int -> 'a -> unit = "%array_safe_set"
(** [set a n x] modifies array [a] in place, replacing
   element number [n] with [x].
   You can also write [a.(n) <- x] instead of [set a n x].

   @raise Invalid_argument
   if [n] is outside the range 0 to [length a - 1]. *)
(*@ set arr n a
    raises Invalid_argument _ -> n < 0 \/ n >= arr.length
    modifies arr.contents
    ensures forall i. 0 <= i < arr.length -> i <> n -> Seq.get arr.contents i = Seq.get (old arr.contents) i
    ensures Seq.get arr.contents n = a *)

external make : int -> 'a -> 'a t = "caml_make_vect"
(** [make n x] returns a fresh array of length [n],
   initialized with [x].
   All the elements of this new array are initially
   physically equal to [x] (in the sense of the [==] predicate).
   Consequently, if [x] is mutable, it is shared among all elements
   of the array, and modifying [x] through one of the array entries
   will modify all other entries at the same time.

   @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
   If the value of [x] is a floating-point number, then the maximum
   size is only [Sys.max_array_length / 2].*)

val sub : 'a t -> int -> int -> 'a t
(** [sub a pos len] returns a fresh array of length [len],
   containing the elements number [pos] to [pos + len - 1]
   of array [a].

   @raise Invalid_argument if [pos] and [len] do not
   designate a valid subarray of [a]; that is, if
   [pos < 0], or [len < 0], or [pos + len > length a]. *)
(*@ res = sub arr pos len
    raises Invalid_argument _ -> pos < 0 \/ len < 0 \/ pos + len > arr.length
    ensures res.length = len
    ensures forall i. 0 <= i < len -> Seq.get res.contents i = Seq.get arr.contents (i + pos) *)

val copy : 'a t -> 'a t
(** [copy a] returns a copy of [a], that is, a fresh array
   containing the same elements as [a]. *)
(*@ res = copy arr
    ensures res.length = arr.length
    ensures res.contents = arr.contents *)

val fill : 'a t -> int -> int -> 'a -> unit
(** [fill a pos len x] modifies the array [a] in place,
   storing [x] in elements number [pos] to [pos + len - 1].

   @raise Invalid_argument if [pos] and [len] do not
   designate a valid subarray of [a]. *)
(*@ fill arr pos len x
    raises Invalid_argument _ -> pos < 0 \/ len < 0 \/ pos + len > arr.length
    modifies arr.contents
    ensures forall i. pos <= i < pos + len -> Seq.get arr.contents i = x
    ensures forall i. 0 <= i < arr.length -> i < pos \/ i >= pos + len -> Seq.get arr.contents i = Seq.get (old arr.contents) i *)

val to_list : 'a t -> 'a list
(** [to_list a] returns the list of all the elements of [a]. *)
(*@ l = to_list arr
    ensures List.length l = arr.length
    ensures forall i. 0 <= i < arr.length -> List.nth l i = Seq.get arr.contents i *)

val mem : 'a -> 'a t -> bool
(** [mem a set] is true if and only if [a] is structurally equal
    to an element of [l] (i.e. there is an [x] in [l] such that
    [compare a x = 0]).
    @since 4.03.0 *)
(*@ b = mem a arr
    ensures b = Seq.mem arr.contents a *)

