(*---------------------------------------------------------------------------
   Copyright (c) 2016 The pvec programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Persistent vectors for OCaml.

    [Pvec] provides persistent, indexed, collections of elements with
    efficient indexing, concatenation and subvector extraction
    operations.

    {b Terminology.} Given a vector [v] we write [v.(i)] the element
    indexed by [i] in [v]. Indices are zero-based and the {e valid
    indices} of a vector [v] of length [l] are the integers in the
    range \[[0];[l-1]\]. Index [0] is the {e first index} and this end
    point is the {e left} of a vector. Index [l-1] is the {e last
    index} and this end point is the {e right} of a vector.

    {b Warning.} All functions that produce larger vectors from smaller
    ones raise [Invalid_argument] if the result is longer than
    {!max_length}.

    {b FIXME.}
    {ul
    {- Iterators from [first] to [last] could be useful.}
    {- Element predicates it would be nice to always pass the index to
       predicates but this prevents easy reuse of (elt -> bool) predicates
       on the vector elements. A wrapping combinator could be added
       but in practice it seems one would need to use it most of the time.
       Doubling all functions with [i] is out of the question -- or
       not it seems this add ~10 funs and is useful in practice}
    {- Provide functor on leaves ? Could be useful for well packed
       structures (e.g. a pre-applied [Pbytes] with strings and/or
       an infinite number of Pt with
       {{:https://github.com/ocaml/ocaml/pull/616}this}).
       What is nice though without functor is the uniformity e.g.  for
       higher order vectors, you always only work with [Pvec]
       regardless. Also see w.r.t. [Uchar.t] plans.}
    {- Implement relaxation and try to keep all ops symmetric.}
    {- Still provide a transient {!Buffer}-like for fast, repeated
       pre/append ?}
    {- Traversals add [first] [last] optional arguments ?}}

    {b References.} N.B. for now not relaxed.
    {ul
    {- N. Stucki et al.
    {e {{:http://dx.doi.org/10.1145/2784731.2784739}
    RRB Vector: A Practical General Purpose Immutable Sequence}}. ICFP 2015.}}

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:pvecs Persistent vectors} *)

val max_length : int
(** [max_length] is the maximal vector length. Note that this
    is larger than both {!Sys.max_string_length} and {!Sys.max_array_length}. *)

type 'a t
(** The type for persistent vectors indexing elements of type ['a]. *)

val length : 'a t -> int
(** [length v] is the number of elements in [v]. *)

(** {1:cons Constructors} *)

val empty : 'a t
(** [empty] is the empty vector. *)

val v : len:int -> 'a -> 'a t
(** [v len e] is a vector [u] of length [len] with [u.(i) = e] for all
    valid indices [i] of [u]. @raise Invalid_argument if [len] is
    not in the range \[[0]; {!max_length}\]. *)

val init : len:int -> (int -> 'a) -> 'a t
(** [init ~len f] is a vector [v] of length [len] with [v.(i) = f i] for
    all valid indices [i] of [v]. @raise Invalid_argument if [len] is
    not in the range \[[0]; {!max_length}\]. *)

val singleton : 'a  -> 'a t
(** [singleton e] is [v ~len:1 e]. *)

val ints : ?start:int -> int -> int t
(** [ints n] is the sequence of integers from [start] (defaults to
    [0]) to [n] (included). This is {!empty} if [start > n]. *)

(** {1:preds Predicates and comparisons} *)

val is_empty : 'a t -> bool
(** [is_empty v] is [true] iff [length v = 0]. *)

val is_filled : 'a t -> int -> bool
(** [is_filled v i] is [true] iff [v.(i)] exists. *)

val is_prefix : eq:('a -> 'a -> bool) -> affix:'a t -> 'a t -> bool
(** [is_prefix ~eq ~affix v] is [true] iff [affix] is a prefix of [v]
    that is iff [eq affix.(i) v.(i)] = [true] for all valid indices
    [i] of [affix]. *)

val is_infix : eq:('a -> 'a -> bool) -> affix:'a t -> 'a t -> bool
(** [is_infix ~eq ~affix v] is [true] iff [affix] can be found in [v]
    that is iff there exists an index [j] in [v] such that for all
    valid indices [i] of [affix], [eq affix.(i) v.(j + i)] =
    [true]. *)

val is_suffix : eq:('a -> 'a -> bool) -> affix:'a t -> 'a t -> bool
(** [is_suffix ~eq ~affix v] is [true] iff [affix] is a suffix of [v]
    that is iff [eq affix.(n - i) v.(m - i)] = [true] for all indices
    [i] of [affix] with [n = length affix - 1] and [m = last_index
    v]. *)

val for_alli : (int -> 'a -> bool) -> 'a t -> bool
(** [for_alli p v] is [true] iff for all indices [i] of [v],
    [p i v.(i) = true]. *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p v] is [for_alli (fun _ e -> p e) v] *)

val existsi : (int -> 'a -> bool) -> 'a t -> bool
(** [existsi p] is [true] iff there exists an index [i] of [v] with [p i
    v.(i) = true]. *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p] is [true] iff there exists an index [i] of [v] with [p
    v.(i) = true]. *)

val equal : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal ~eq u v] is [true] iff [u] and [v] have the same length
    and for all indices [i] of [u], [eq u.(0) v.(1)]. *)

val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare ~cmp u v] is the lexicographic order betwen [u] and [v]
    using [cmp] to compare elements. *)

(** {1:get Getting elements} *)

val get : 'a t -> int -> 'a
(** [get v i] is [v.(i)]. @raise Invalid_argument if [i] is not a
    valid index of [v]. *)

val get_first : 'a t -> 'a
(** [get_first v] is [get v 0]. *)

val get_last : 'a t -> 'a
(** [get_last v] is [get v (length v - 1)]. *)

val el : 'a t -> int -> 'a option
(** [el v i] is the element [v.(i)], if any; in particular this is
    [None] on [i < 0]. *)

val first_el : 'a t -> 'a option
(** [first_el v] is [el v 0]. *)

val last_el : 'a t -> 'a option
(** [last_el v] is [el v (length v - 1)]. *)

val range : first:int -> last:int -> 'a t -> 'a t
(** [range ~first ~last v] is [u] defined as the consecutive elements
    of [v] whose indices exist in the range \[[first];[last]\], in the
    same order. [first] and [last] can be any integer.  If the index
    range does not exist in [v] or if [first > last], [u] is empty. *)

(** {1:add Adding elements} *)

val append : 'a t -> 'a t -> 'a t
(** [append v u] is the vector resulting from appending [u] to [v]. *)

val ( ++ ) : 'a t -> 'a t -> 'a t
(** [v ++ u] is [append v u]. *)

val add_first : 'a -> 'a t -> 'a t
(** [add_first e v] is [singleton e ++ v]. *)

val add_last : 'a t -> 'a -> 'a t
(** [add_last v e] is [v ++ singleton e]. *)

val concat : ?sep:'a t -> 'a t t -> 'a t
(** [concat ~sep vs] is the concatenation of the vectors of [vs], separated
    by [sep] (defaults to {!empty}):
    [vs.(0) ++ sep ++ vs.(2) ++] ... [vs.(length v - 1)] or {!empty} if
    [vs] is empty. *)

val concat_list : ?sep:'a t -> 'a t list -> 'a t
(** [concat_list ~sep vs] is [concat ~sep (of_list vs)]. *)

val splice : ?last:int -> into:'a t -> first:int -> 'a t -> 'a t
(** [splice ~into ~first ~last v] replaces the elements of [into] in
    range \[[first]; [last]\] with those of [v] or inserts [v] at
    [first] if the range is empty. [last] defaults to [first - 1], the
    range is empty. More precisely this is:
{ul
{- [take_left first into ++ v ++ drop_left (last + 1) into] if [first <= last].}
{- [take_left first into ++ v ++ drop_left first into] otherwise.}}
    By definition if [last < first] inserts [v] at [first], pushing
    elements of [into] starting at [first] on the right. Also by definition,
    if the specified range does not overlap [into]'s range this prepends or
    appends [v] to [into] accordingly. *)

(** {1:set Setting elements} *)

val set : 'a t -> int -> 'a -> 'a t
(** [set v i e] is [v] with the element at index [i] equal to [e].

    @raise Invalid_argument if [i] is not a valid index of [v]. *)

val set_first : 'a t -> 'a -> 'a t
(** [set_first v e] is [set v 0 e]. *)

val set_last : 'a t -> 'a -> 'a t
(** [set_last v e] is [set v (length v - 1) e]. *)

val fill : pad:'a -> 'a t -> int -> 'a -> 'a t
(** [fill ~pad u i e] is:
{ul
{- [set u i e] if [i] is a valid index of [u].}
{- [set (u ++ (v len:(i + 1 - length v) pad)) i e] otherwise.}}

    @raise Invalid_argument if [i] is negative. *)

val fill_first : 'a t -> 'a -> 'a t
(** [fill_first v e] is [set v 0 e] or [singleton e] if [v] is empty. *)

val fill_last : 'a t -> 'a -> 'a t
(** [fill_last v e] is [set v (length v - 1) e] or [singleton e] if
    [v] is empty. *)

(** {1:rem Removing elements} *)

val rem_range : first:int -> last:int -> 'a t -> 'a t
(** [rem_range ~first ~last v] is [v] without the elements of [v]
    that exist in the range \[[first];[last]\]. More precisely this is:
{ul
{- [take_left first v ++ drop_left (last + 1) v] if [first <= last].}
{- [v] otherwise.}}
    by definition if the specified range is empty or doesn't overlap
    with [v]'s valid indices this is [v]. *)

val rem : 'a t -> int -> 'a t
(** [rem v i] is [rem_range ~first:i ~last:i v]. *)

val rem_first : 'a t -> 'a t
(** [rem_first v] is [rem v 0]. *)

val rem_last : 'a t -> 'a t
(** [rem_last v] is [rem v (length v - 1)]. *)

(** {1:traverse Traversing} *)

val foldi_left : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [foldi_left f acc v] is [f (... (f (f acc 0 v.(0)) 1 v.(1)) ...) m s.(m)]
    with [m = length v - 1] or [acc] if [v] is empty. *)

val foldi_right : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold_right f v acc] is [f 0 v.(0) (f 1 v.(1) (... (f m v.(m) acc) ..))]
    with [m = length v - 1] or [acc] if [v] is empty. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_left f acc v] is [foldi_left (fun acc _ e -> f acc) acc v]. *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold_right f v acc] is [foldi_right (fun _ e acc -> f acc) v acc]. *)

val iteri_left : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri_left f v] is [f 0 v.(0); f 1 v.(1); ...; f m v.(m)] with
    [m = length v - 1] or [()] if [v] is empty. *)

val iteri_right : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri_right f v] is [f m v.(m); f (m-1) v.(m-1); ...; f 0 v.(0)] with
    [m = length v - 1] or [()] if [v] is empty. *)

val iter_left : ('a -> unit) -> 'a t -> unit
(** [iter_left f v] is [iteri_left (fun _ e -> f e) v]. *)

val iter_right : ('a -> unit) -> 'a t -> unit
(** [iter_right f v] is [iteri_right (fun _ e -> f e) v]. *)

(** {1:mapping Mapping and rearranging} *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f v] is [u] with [u.(i) = f i v.(i)] for all indices [i] of
    [v] or the empty vector if [v] is empty. No guarantee is provided
    in the invocation order of [f] on elements. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f v] is [mapi (fun _ e -> f e) v]. *)

val filter_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b t
(** [filter_map f s] is the vector made of the elements of [v] as
    (and if) mapped by [f], in the same order. No guarantee is provided
    on the invocation order of [f] on elements. See also {!filter}. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f s] is [filter_mapi (fun _ e -> f e) v]. *)

val rev : 'a t -> 'a t
(** [rev v] is [u] the vector with elements of [v] reversed, [u.(i) =
    v.(length v - 1 - i)] for all indices [i] of [v] or the empty
    vector if [v] is empty. *)

val indices : 'a t -> int t
(** [indices v] is [mapi (fun i _ -> i)]. *)

val transpose : 'a t t -> 'a t t
(** [transpose v] is [u] such that [u.(i).(j)] = [v.(j).(i)] for all
    indices [i] of [v] and [j] of its subvectors. *)

val stable_sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** [stable_sort compare v] is [v] with its elements sorted according to
    [compare]. The sort is stable, elements that compare equal are kept
    in their original order. *)

val sort_uniq : cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** [sort_uniq compare v] is like {!stable_sort} except duplicates
    are removed. *)

val shuffle : rand:(int -> int) -> 'a t -> 'a t
(** [shuffle rand v] is a random permutation of [v]'s elements.
    [rand] must be such that [rand n] is a random number in the range
    \[[0];[n-1]\]. *)

val unstutter : eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** [unstutter ~eq v] is [v] without consecutive equal elements. *)

(** {1:breaking Breaking} *)

(** {2:mag_break Breaking with magnitudes} *)

val take_left : int -> 'a t -> 'a t
(** [take_left n v] has the first [n] elements of [v]. By definition
    the result is {!empty} if [n] is negative and [v] if [n >= length v]. *)

val take_right : int -> 'a t -> 'a t
(** [take_right n v] has the last [n] elements of [v]. By definition
    the result is {!empty} if [n] is negative and [v] if [n >= length v].  *)

val drop_left : int -> 'a t -> 'a t
(** [drop_left n v] has the elements of [v] without the first [n] elements.
    By definition the result is [v] if [n] is zero or negative and
    {!empty} if [n >= length v]. *)

val drop_right : int -> 'a t -> 'a t
(** [drop_right n v] has the elements of [v] without the last [n] elements.
    By definition the result is [v] if [n] is zero or negative and
    {!empty} if [n >= length v]. *)

val break_left : int -> 'a t -> 'a t * 'a t
(** [break_left n v] is [(take_left n v, drop_left n v)]. *)

val break_right : int -> 'a t -> 'a t * 'a t
(** [break_right n v] is [(drop_right n v, take_right n v)]. *)

val chunk_left : int -> 'a t -> 'a t t
(** [chunk_left size v] is [v], in order, chunked into vectors of
    exactly [size] elements except maybe for the last one. If [length
    v <= size] this is [singleton v].

    @raise Invalid_argument if [size] is not strictly positive. *)

val chunk_right : int -> 'a t -> 'a t t
(** [chunk_right size v] is [v], in order, chunked into vectors of
    exactly [size] elements except maybe for the first one. If
    [length v <= size] this is [singleton v].

    @raise Invalid_argument if [size] is not strictly positive. *)

val pop_first : 'a t -> ('a * 'a t) option
(** [pop_first v] is [Some (get_first v, drop_left 1 v)] or [None]
    if [v] is empty. *)

val pop_last : 'a t -> ('a t * 'a) option
(** [pop_last v] is [Some (drop_right 1 v,  get_last v)] or [None]
    if [v] is empty. *)

(** {2:pred_break Breaking with predicates} *)

val keepi_left : (int -> 'a -> bool) -> 'a t -> 'a t
(** [keep_left sat v] has the first consecutive [sat] satisfying
    elements of [v]. *)

val keepi_right : (int -> 'a -> bool) -> 'a t -> 'a t
(** [keepi_right sat v] has the last consecutive [sat] satisfying
    elements of [v]. *)

val keep_left : ('a -> bool) -> 'a t -> 'a t
(** [keep_left sat v] is [keepi_left (fun _ e -> sat e) v]. *)

val keep_right : ('a -> bool) -> 'a t -> 'a t
(** [keep_right sat v] is [keepi_right (fun _ e -> sat e) v]. *)

val losei_left : (int -> 'a -> bool) -> 'a t -> 'a t
(** [lose_left sat v] has the elements of [v] without the first
    consecutive [sat] satisfying elements of [v]. *)

val losei_right : (int -> 'a -> bool) -> 'a t -> 'a t
(** [lose_right sat v] has the elements of [v] without the last
    consecutive [sat] satisfying elements of [v]. *)

val lose_left : ('a -> bool) -> 'a t -> 'a t
(** [lose_left sat v] is [loosei_left (fun _ e -> sat e) v]. *)

val lose_right : ('a -> bool) -> 'a t -> 'a t
(** [lose_right sat v] is [loosei_right (fun _ e -> sat e) v]. *)

val spani_left : (int -> 'a -> bool) -> 'a t -> 'a t * 'a t
(** [span_left sat v] is [(keepi_left sat v, losei_left sat v)]. *)

val spani_right : (int -> 'a -> bool) -> 'a t -> 'a t * 'a t
(** [span_right sat v] is [(losei_right sat v, keepi_right sat v)]. *)

val span_left : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [span_left sat v] is [spani_left (fun _ e -> sat e) v]. *)

val span_right : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [span_right sat v] is [spani_right (fun _ e -> sat e) v]. *)

val trimi : (int -> 'a -> bool) -> 'a t -> 'a t
(** [trimi drop v] is [losei_right drop (losei_left drop v)]. *)

val trim : ('a -> bool) -> 'a t -> 'a t
(** [trim drop v] is [trim (fun _ e -> drop e) v]. *)

(** {2:cuts Breaking with separators} *)

val cut_left : sep:'a t -> 'a t -> ('a t * 'a t) option
(** [cut_left sep v] is either the pair [Some (l, r)] of the two
    (possibly empty) vectors of [v] that are delimited by the first
    match of the non empty separator vector [sep] or [None] if [sep]
    can't be matched in [v]. Matching starts from the left of
    [v].

    The invariant [concat [l; sep; r] = v] holds. @raise Invalid_argument
    if [sep] is empty. *)

val cut_right : sep:'a t -> 'a t -> ('a t * 'a t) option
(** [cut_right sep v] is like {!cut_right} but matching starts on the right. *)

val cuts_left : ?drop_empty:bool -> sep:'a t -> 'a t -> 'a t t
(** [cuts_left ~drop_empty ~sep v] is the vector of subvectors of [v] that
    are delimited by matches of the non empty separator vector [sep]. Empty
    vectors are omitted in the vector if [drop_empty] is [true] (defaults
    to [false]).

    Matching separators in [v] starts from the begining of [v]. Once one
    is found, the separator is skipped and matching starts again, that
    is separator matches can't overlap. If there is no separator match in
    [v] the vector [singleton v] is returned.

    The following invariants hold:

    {ul
    {- [concat ~sep (left_cuts ~drop_empty:false ~sep v) = v]}
    {- [left_cuts ~drop_empty:false ~sep s <> empty]}}

    @raise Invalid_argument if [sep] is empty. *)

val cuts_right : ?drop_empty:bool -> sep:'a t -> 'a t -> 'a t t
(** [cuts_right ~drop_empty ~sep v] is like {!cuts_left} but matching
    starts from the right. *)

val fields : ?drop_empty:bool -> is_sep:('a -> bool) -> 'a t -> 'a t t
(** [fields ~drop_empty ~is_sep v] is the vector of (possibly empty)
    subvectors that are delimited by elements for which [is_sep] is [true].
    Empty subvectors are omitted in the vector if [drop_empty] is [true]
    (defaults to [false]). *)

(** {1:search_select Searching and selecting elements} *)

val left_find : ?start:int -> ('a -> bool) -> 'a t -> (int * 'a) option
(** [left_find ~start sat v] is [Some (i, v.(i))] with [i] the
    smallest index [i], if any, greater or equal to [start] such that
    [sat v.(i)] is [true]. [start] defaults to [0]. By definition
    if [start < 0] the search starts at [0] and if [i > length v - 1]
    [None] is returned. *)

val right_find : ?start:int -> ('a -> bool) -> 'a t -> (int * 'a) option
(** [right_find ~start sat v] is [Some (i, v.(i))] with [i] the
    greatest index [i], if any, smaller or equal to [start] such that
    [sat v.(i)] is [true]. [start] defaults to [length v - 1]. By definition
    if [start < 0], [None] is returned and if [start > length v - 1] the
    search starts at [length v - 1]. *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [partition sat v] is a pair of vectors [(vt, vf)] with
    [vt] the elements that satisfy [sat] and [vf] the elements
    that do not. In both vectors the order is preserved. No guarantee is
    provided on the invocation of [f] on elements. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter sat v] is [fst (partition sat v)]. *)

(** {1:edit_dist Edit distance} *)

val edit_distance : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> int
(** [edit_distance ~eq u v] is the number of single element edits
    (insertion, deletion, substitution) that are needed to change
    [u] into [v]. *)

val suggest : ?dist:int -> eq:('a -> 'a -> bool) -> 'a t t -> 'a t -> 'a t t
(** [suggest ~dist candidates v] are the elements of [candidates]
    whose {!edit_distance} is the smallest to [s] and at most
    at distance [dist] of [s] (defaults to [2]). If multiple
    results are returned the order of [candidates] is preserved. *)

(** {1:conv Conversions}

    {b FIXME.} Add conversion from/to bigarrays and [buffer_add]. *)

val of_list : 'a list -> 'a t
(** [of_list l] is [l] as a persistent vector.

    @raise Invalid_argument if [List.length l > Pvec.max_length]. *)

val to_list : 'a t -> 'a list
(** [to_list v] is [v] as a list. *)

val of_array : 'a array -> 'a t
(** [of_array a] is [a] as a persistent vector. *)

val to_array : 'a t -> 'a array
(** [to_array v] is [v] as an array.

    @raise Invalid_argument if [Pvec.length v > Sys.max_array_length] *)

val of_string : string -> char t
(** [of_string s] is [s] as a persistent vector. *)

val to_string : char t -> string
(** [to_string v] is [v] as a string.

    @raise Invalid_argument if [Pvec.length v > Sys.max_array_length] *)

val of_bytes : bytes -> char t
(** [of_bytes bytes] is [bytes] as a persistent vector. *)

val to_bytes : char t -> bytes
(** [to_string v] is [v] as bytes

    @raise Invalid_argument if [Pvec.length v > Sys.max_array_length] *)

(** {1:print Pretty printing} *)

val pp :
  ?sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp ~sep pp_v ppf v] formats [v]'s elements. Each element of the
    vector is formatted in order with [pp_v]. Elements are separated
    by [sep] (defaults to {!Format.pp_print_cut}). If the vector is
    empty [ppf] is left untouched. *)

val pp_chars : Format.formatter -> char t -> unit
(** [pp_chars] is [pp ~sep:(fun _ _ -> ()) Format.pp_print_char]. *)

val dump : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t ->
  unit
(** [dump pp_v ppf v] prints an unspecified representation of [v] on [ppf]
    using [pp_v] to print the elements. *)

(**/**)
val branching : int
(** [branching] is the internal branching factor. *)
(**/**)

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
