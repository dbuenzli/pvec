(*---------------------------------------------------------------------------
   Copyright (c) 2016 The pvec programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Pvec's kernel operations on which all other are expressed.
   Note that these operations do not perform bound checks. *)

module Leaf : sig
  type 'a t
  val v : len:int -> 'a -> 'a t
  val init : len:int -> (int -> 'a) -> 'a t
  val singleton : 'a -> 'a t
  val length : 'a t -> int
  val copy : 'a t -> 'a t
  val get : 'a t -> int -> 'a
  val pset : 'a t -> int -> 'a -> 'a t
  val mset : 'a t -> int -> 'a -> unit
  val mblit : src:'a t -> int -> dst:'a t -> int -> len:int -> unit
  val iter_left : ('a -> unit) -> 'a t -> unit
  val iter_right : ('a -> unit) -> 'a t -> unit
  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : ('a -> 'b) -> 'a t -> 'b t
  val of_sub_array : 'a array -> int -> len:int -> 'a t
  val blit_array : 'a t -> 'a array -> int -> len:int -> unit
end

val max_length : int
val branching_factor : int

type 'a t
val length : 'a t -> int

val empty : 'a t
val v : len:int -> 'a -> 'a t
val init_leaves : len:int -> (zero:int -> len:int -> 'a Leaf.t) -> 'a t
val init : len:int -> (int -> 'a) -> 'a t
val singleton : 'a -> 'a t
val copy : 'a t -> 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> 'a t
val mset : 'a t -> int -> 'a -> unit
val add_first : 'a -> 'a t -> 'a t
val add_last : 'a t -> 'a -> 'a t
val append : 'a t -> 'a t -> 'a t
val range : first:int -> last:int -> 'a t -> 'a t
val break : left_len:int -> 'a t -> 'a t * 'a t
val iter_left : ('a -> unit) -> 'a t -> unit
val iter_right : ('a -> unit) -> 'a t -> unit
val iteri_left : (int -> 'a -> unit) -> 'a t -> unit
val iteri_right : (int -> 'a -> unit) -> 'a t -> unit
val iteri_left_leaves : (int -> 'a Leaf.t -> 'b) -> 'a t -> unit
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val map : ('a -> 'b) -> 'a t -> 'b t

(* The following two functions assume v0 and v1 are of the same length *)

val equal : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The pvec progammers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
