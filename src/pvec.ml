(*---------------------------------------------------------------------------
   Copyright (c) 2016 The pvec programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let max_length = Pveck.max_length
let branching_factor = Pveck.branching_factor

(* Errors *)

let invalidf fmt = Printf.ksprintf (fun s -> invalid_arg s) fmt
let err_length l = invalidf "length %d not in [0;%d]" l max_length
let err_index i = function
| 0 -> invalidf "indexing empty vector with %d" i
| len -> invalidf "index %d not in [0;%d]" i (len - 1)

let err_neg i = invalidf "negative index (%d)" i
let err_empty_sep () = invalidf "~sep is an empty string"
let err_max_length () = invalidf "maximum length %d exceeded" max_length
let err_array len =
  invalidf "vector too large (%d) to fit in an array (max %d)"
    len Sys.max_array_length

let err_string len =
  invalidf "vector too large (%d) to fit in a string (max %d)"
    len Sys.max_string_length

let err_chunk size =
  invalidf "chunk size %d is not strictly positive" size

(* Persistent vectors *)

type 'a t = 'a Pveck.t
let length = Pveck.length

(* Constructors *)

let empty = Pveck.empty

let v ~len e =
  if 0 <= len && len <= max_length then Pveck.v ~len e else err_length len

let init ~len el =
  if 0 <= len && len <= max_length then Pveck.init ~len el else err_length len

let singleton = Pveck.singleton

let ints ?(start = 0) n = match start > n with
| true -> empty
| false -> init ~len:(n + 1 - start) (fun i -> start + i)

(* Predicates and comparisons *)

let is_empty v = length v = 0
let is_filled v i = 0 <= i && i <= length v - 1
let is_prefix ~eq ~affix v =
  (* +++ Would be nice to use some kind of iter2 rather than `get`ing *)
  let len_a = length affix in
  let len_v = length v in
  if len_a > len_v then false else
  let rec loop affix v last i = match i > last with
  | true -> true
  | false ->
      let ai = Pveck.get affix i in
      let vi = Pveck.get v i in
      eq ai vi && loop affix v last (i + 1)
  in
  loop affix v (len_a - 1) 0

let is_infix ~eq ~affix v =
  (* +++ Would be nice to use some kind of iter2 rather than `get`ing *)
  let len_a = length affix in
  let len_v = length v in
  if len_a > len_v then false else
  let rec loop affix v last_a max_v i k = match i > max_v with
  | true -> false
  | false ->
      match k > last_a with
      | true -> true
      | false ->
          let ai = Pveck.get affix k in
          let vi = Pveck.get v (i + k) in
          if eq ai vi
          then loop affix v last_a max_v i (k + 1)
          else loop affix v last_a max_v (i + 1) 0
  in
  let last_a = len_a - 1 in
  let max_v = len_v - len_a in
  loop affix v last_a max_v 0 0

let is_suffix ~eq ~affix v =
  (* +++ Would be nice to use some kind of iter2 rather than `get`ing *)
  let last_a = length affix - 1 in
  let last_v = length v - 1 in
  if last_a > last_v then false else
  let rec loop affix v last_a last_v i = match i > last_a with
  | true -> true
  | false ->
      let ai = Pveck.get affix (last_a - i) in
      let vi = Pveck.get v (last_v - i) in
      eq ai vi && loop affix v last_a last_v (i + 1)
  in
  loop affix v last_a last_v 0

let for_all p v =
  if length v = 0 then false else
  let check e = if p e then () else raise_notrace Exit in
  try Pveck.iter_left check v; true with Exit -> false

let for_alli p v =
  if length v = 0 then false else
  let check i e = if p i e then () else raise_notrace Exit in
  try Pveck.iteri_left check v; true with Exit -> false

let exists p v =
  if length v = 0 then false else
  let find e = if p e then raise_notrace Exit else () in
  try Pveck.iter_left find v; false with Exit -> true

let existsi p v =
  if length v = 0 then false else
  let find i e = if p i e then raise_notrace Exit else () in
  try Pveck.iteri_left find v; false with Exit -> true

let equal ~eq v0 v1 = match length v0 = length v1 with
| false -> false
| true -> Pveck.equal ~eq v0 v1

let compare ~cmp v0 v1 = match compare (length v0) (length v1) with
| (-1 | 1) as i -> i
| 0 -> Pveck.compare ~cmp v0 v1
| _ -> assert false

(* Getting elements *)

let get v i =
  if 0 <= i && i < length v then Pveck.get v i else err_index i (length v)

let get_first v = get v 0
let get_last v = get v (length v - 1)

let el v i = if 0 <= i && i < length v then Some (Pveck.get v i) else None
let first_el v = if length v = 0 then None else Some (get_first v)
let last_el v = if length v = 0 then None else Some (get_last v)

let range ~first ~last v =
  let len = length v in
  match first > last || first >= len || last < 0 with
  | true -> empty
  | false ->
      let first = if first < 0 then 0 else first in
      let last = if last >= len then len - 1 else last in
      Pveck.range ~first ~last v

(* Adding *)

let append v0 v1 = match length v0 + length v1 > max_length with
| true -> err_max_length ()
| false -> Pveck.append v0 v1

let ( ++ ) = append

let add_first = Pveck.add_first
let add_last = Pveck.add_last

let concat ?(sep = empty) vs = match length vs with
| 0 -> empty
| _ ->
    (* +++ something better than this can be done e.g. with a streaming
       perfect balance *)
    let add acc v = acc ++ sep ++ v in
    let first = get_first vs in
    let t = (* +++ avoid *) Pveck.range ~first:1 ~last:(length vs - 1) vs in
    Pveck.fold_left add first t

let concat_list ?(sep = empty) = function
| [] -> empty
| v :: vs ->
    (* +++ see concat *)
    List.fold_left (fun acc v -> acc ++ sep ++ v) v vs

let splice ?last ~into ~first v =
  let last = match last with None -> first - 1 | Some last -> last in
  let into_last = length into - 1 in
  match first <= last with
  | true ->
      if last < 0 then v ++ into else
      if first > into_last then into ++ v else
      let l = match first <= 0 with
      | true -> empty
      | false -> Pveck.range ~first:0 ~last:(first - 1) into
      in
      let r = match last + 1 > into_last with
      | true -> empty
      | false -> Pveck.range ~first:(last + 1) ~last:into_last into
      in
      l ++ v ++ r
  | false ->
      if first < 0 then v ++ into else
      if first > into_last then into ++ v else
      let left, right = Pveck.break first into in
      left ++ v ++ right

(* Setting elements *)

let set v i e =
  if 0 <= i && i < length v then Pveck.set v i e else err_index i (length v)

let set_first v e = set v 0 e
let set_last v e = set v (length v - 1) e

let fill ~pad u i e = match i < 0 with
| true -> err_neg i
| false ->
    match 0 <= i && i < length u with
    | true -> Pveck.set u i e
    | false ->
        let i = i - length u in
        let adds = v ~len:(i + 1) pad in
        Pveck.mset adds i e;
        u ++ adds

let fill_first v e = if length v = 0 then singleton e else set_first v e
let fill_last v e = if length v = 0 then singleton e else set_last v e

(* Removing *)

let rem_range ~first ~last v =
  if first > last then v else
  let last_v = length v - 1 in
  if last < 0 then v else
  if first > last_v then v else
  if first <= 0 then range ~first:(last + 1) ~last:last_v v else
  if last >= last_v then range ~first:0 ~last:(first - 1) v else
  range ~first:0 ~last:(first - 1) v ++
  range ~first:(last + 1) ~last:last_v v

let rem v i = rem_range ~first:i ~last:i v
let rem_first v = rem v 0
let rem_last v = rem v (length v - 1)

(* Traversing *)

let acci_left f = let i = ref (-1) in fun acc e -> incr i; f acc !i e
let acci_right f len = let i = ref len in fun e acc -> decr i; f !i e acc

let fold_left = Pveck.fold_left
let fold_right = Pveck.fold_right
let foldi_left f acc v = fold_left (acci_left f) acc v
let foldi_right f v acc = fold_right (acci_right f (length v)) v acc
let iter_left = Pveck.iter_left
let iter_right = Pveck.iter_right
let iteri_left = Pveck.iteri_left
let iteri_right = Pveck.iteri_right

(* Mapping and rearranging elements *)

let funci_left f = let i = ref (-1) in fun e -> incr i; f !i e

let map = Pveck.map
let mapi f v = map (funci_left f) v (* assumes left-to-right Pveck.map *)

let filter_map f v =
  if length v = 0 then empty else
  let fmap f acc e = match f e with None -> acc | Some v -> add_last acc v in
  Pveck.fold_left (fmap f) empty v

let filter_mapi f v = filter_map (funci_left f) v

let rev v =
  (* +++ avoid tree walks ? *)
  if length v = 0 then empty else
  let v = Pveck.copy v in
  let last = length v - 1 in
  for i = 0 to last / 2 do
    let j = last - i in
    let t = Pveck.get v i in
    Pveck.mset v i (Pveck.get v j);
    Pveck.mset v j t;
  done;
  v

let indices v = mapi (fun i _ -> i) v
let transpose v = failwith "TODO"
let stable_sort ~cmp v = failwith "TODO"
let sort_uniq ~cmp v = failwith "TODO"

let shuffle ~rand v = (* Fisher-Yates *)
  (* +++ avoid tree walks ? *)
  let v = Pveck.copy v in
  let rswap i ei =
    let j = rand (i + 1) in
    Pveck.mset v i (get v j);
    Pveck.mset v j ei
  in
  iteri_right rswap v; v

let unstutter ~eq v =
  if length v = 0 then empty else
  let f eq (prev, unst as acc) e = match eq prev e with
  | true -> acc
  | false -> (e, add_last unst e)
  in
  let first = get_first v in
  let acc = first, singleton first in
  snd (Pveck.fold_left (f eq) acc v)

(* Breaking with magnitudes *)

let take_left n v = match n with
| n when n <= 0 -> empty
| n when n >= length v -> v
| _ -> Pveck.range ~first:0 ~last:(n - 1) v

let take_right n v = match n with
| n when n <= 0 -> empty
| n when n >= length v -> v
| _ -> Pveck.range ~first:(length v - n) ~last:(length v - 1) v

let drop_left n v = match n with
| n when n <= 0 -> v
| n when n >= length v -> empty
| _ -> Pveck.range ~first:n ~last:(length v - 1) v

let drop_right n v = match n with
| n when n <= 0 -> v
| n when n >= length v -> empty
| _ -> Pveck.range ~first:0 ~last:(length v - n - 1) v

let break_left n v = match n with
| n when n <= 0 -> empty, v
| n when n >= length v -> v, empty
| _ -> Pveck.break ~left_len:n v

let break_right n v = match n with
| n when n <= 0 -> v, empty
| n when n >= length v -> empty, v
| _ -> Pveck.break ~left_len:(length v - n) v

let chunk ~first_is_small size v =
  if size <= 0 then err_chunk size else
  let v_len = length v in
  if v_len <= size then singleton v else
  let chunk_count, rem_size = match v_len mod size with
  | 0 -> v_len / size, size
  | r -> v_len / size + 1, r
  in
  let el_first_small i =
    let first, len = match i = 0 with
    | true -> 0, rem_size
    | false -> (rem_size + (i - 1) * size), size
    in
    range ~first ~last:(first + len - 1) v
  in
  let el_last_small i =
    let first = i * size in
    let len = if i <> chunk_count - 1 then size else rem_size in
    range ~first ~last:(first + len - 1) v
  in
  let el = if first_is_small then el_first_small else el_last_small in
  Pveck.init ~len:chunk_count el

let chunk_left size v = chunk ~first_is_small:false size v
let chunk_right size v = chunk ~first_is_small:true size v

let pop_first v =
  if length v = 0 then None else Some (get_first v, drop_left 1 v)

let pop_last v =
  if length v = 0 then None else Some (drop_right 1 v, get_last v)

(* Breaking with separators *)

exception Unsat of int
let is_unsat sat i e = if sat e then () else raise_notrace (Unsat i)
let is_unsati sat i e = if sat i e then () else raise_notrace (Unsat i)

let _keep_left is_unsat sat v = try iteri_left (is_unsat sat) v; v with
| Unsat i -> if i = 0 then empty else Pveck.range ~first:0 ~last:(i - 1) v

let keep_left sat v = _keep_left is_unsat sat v
let keepi_left sat v = _keep_left is_unsati sat v

let _keep_right is_unsat sat v = try iteri_right (is_unsat sat) v; v with
| Unsat i ->
    let last = length v - 1 in
    if i = last then empty else Pveck.range ~first:(i + 1) ~last v

let keep_right sat v = _keep_right is_unsat sat v
let keepi_right sat v = _keep_right is_unsati sat v

let _lose_left is_unsat sat v = try iteri_left (is_unsat sat) v; empty with
| Unsat i -> if i = 0 then v else Pveck.range ~first:i ~last:(length v - 1) v

let lose_left sat v = _lose_left is_unsat sat v
let losei_left sat v = _lose_left is_unsati sat v

let _lose_right is_unsat sat v = try iteri_right (is_unsat sat) v; empty with
| Unsat i -> if i = (length v - 1) then v else Pveck.range ~first:0 ~last:i v

let lose_right sat v = _lose_right is_unsat sat v
let losei_right sat v = _lose_right is_unsati sat v

let _span_left unsat sat v = try iteri_left (unsat sat) v; v, empty with
| Unsat i -> break_left i v

let span_left sat v = _span_left is_unsat sat v
let spani_left sat v = _span_left is_unsati sat v

let _span_right unsat sat v = try iteri_right (unsat sat) v; empty, v with
| Unsat i -> break_left (i + 1) v

let span_right sat v = _span_right is_unsat sat v
let spani_right sat v = _span_right is_unsati sat v

let _trim is_unsat drop v = try iteri_left (is_unsat drop) v; empty with
| Unsat first ->
    try iteri_right (is_unsat drop) v; empty with
    | Unsat last ->
        if first = 0 && last = length v - 1 then v else
        Pveck.range ~first ~last v

let trim drop v = _trim is_unsat drop v
let trimi drop v = _trim is_unsati drop v

(* Breaking with separators *)

let cut_left ~sep v = match length sep with
| 0 -> err_empty_sep ()
| sep_len ->
    (* +++ express this using iteration rather than [get]s, at least on [v] *)
    let v_len = length v in
    let sep_last = sep_len - 1 in
    let max_v = v_len - sep_len in
    let rec check_sep i k = match k > sep_last with
    | true ->
        let v_last = v_len - 1 in
        let r_first = i + sep_len in
        let right = match r_first > v_last with
        | true -> empty
        | false -> Pveck.range ~first:r_first ~last:v_last v
        in
        Some (Pveck.range ~first:0 ~last:i v, right)
    | false ->
        match get v (i + k) = get sep k with
        | true -> check_sep i (k + 1)
        | false -> scan (i + 1)
    and scan i = match i > max_v with
    | true -> None
    | false -> if get v i = get sep 0 then check_sep i 1 else scan (i + 1)
    in
    scan 0

let cut_right ~sep v = match length sep with
| 0 -> err_empty_sep ()
| sep_len ->
    (* +++ express this using iteration rather than [get]s, at least on [v] *)
    let v_len = length v in
    let sep_last = sep_len - 1 in
    let v_last = v_len - 1 in
    let rec check_sep i k = match k > sep_last with
    | true ->
        let r_start = i + sep_len in
        let left = match i = 0 with
        | true -> empty
        | false -> Pveck.range ~first:0 ~last:(i - 1) v
        in
        Some (left, Pveck.range ~first:r_start ~last:v_last v)
    | false ->
        match get v (i + k) = get sep k with
        | true -> check_sep i (k + 1)
        | false -> scan (i - 1)
    and scan i = match i < 0 with
    | true -> None
    | false -> if get v i = get sep 0 then check_sep i 1 else scan (i - 1)
    in
    scan (v_last - sep_last)

let cuts_left ?(drop_empty = false) ~sep v =
  let add ~drop_empty v ~first ~last acc = match first = last with
  | true when drop_empty -> acc
  | true -> add_last acc empty
  | false -> add_last acc (Pveck.range ~first ~last v)
  in
  match length sep with
  | 0 -> err_empty_sep ()
  | sep_len ->
      (* +++ express this using iteration rather than [get]s, at least on [v] *)
      let v_len = length v in
      let sep_last = sep_len - 1 in
      let max_v = v_len - sep_len in
      let rec check_sep first i k acc = match k > sep_last with
      | true ->
          let new_start = i + sep_len in
          scan new_start new_start (add ~drop_empty v ~first ~last:(i - 1) acc)
      | false ->
          match get v (i + k) = get sep k with
          | true -> check_sep first i (k + 1) acc
          | false -> scan first (i + 1) acc
      and scan first i acc = match i > max_v with
      | true -> add ~drop_empty v ~first ~last:(i - 1) acc
      | false ->
          match get v i = get sep 0 (* +++ cache this *) with
          | true -> check_sep first i 1 acc
          | false -> scan first (i + 1) acc
      in
      scan 0 0 empty

let cuts_right ?(drop_empty = false) ~sep v =
  let add ~drop_empty v ~first ~last acc = match first = last with
  | true when drop_empty -> acc
  | true -> add_first empty acc
  | false -> add_first (Pveck.range ~first ~last v) acc
  in
  match length sep with
  | 0 -> err_empty_sep ()
  | sep_len ->
      (* +++ express this using iteration rather than [get]'s, at least on [v]*)
      let v_len = length v in
      let sep_last = sep_len - 1 in
      let v_last = v_len - 1 in
      let rec check_sep last i k acc = match k > sep_last with
      | true ->
          let first = i + sep_len in
          scan i (i - sep_len) (add ~drop_empty v ~first ~last acc)
      | false ->
          match get v (i + k) = get sep k with
          | true -> check_sep last i (k + 1) acc
          | false -> scan last (i - 1) acc
      and scan last i acc = match i < 0 with
      | true -> add ~drop_empty v ~first:0 ~last acc
      | false ->
          match get v i = get sep 0 (* +++ cache this *) with
          | true -> check_sep last i 1 acc
          | false -> scan last (i - 1) acc
      in
      scan v_last (v_last - sep_last) empty

let fields ?(drop_empty = false) ~is_sep v =
  let add ~drop_empty v ~first ~last acc = match first = last with
  | true when drop_empty -> acc
  | true -> add_first empty acc
  | false -> add_first (Pveck.range ~first ~last v) acc
  in
  match length v with
  | 0 -> empty
  | len ->
      (* +++ express this using iteration rather than [get]'s *)
      let rec loop i last acc = match i < 0 with
      | true -> add ~drop_empty v ~first:i ~last acc
      | false ->
          match is_sep (Pveck.get v i) with
          | false -> loop (i - 1) last acc
          | true ->
              loop (i - 1) (i - 1) (add ~drop_empty v ~first:(i + 1) ~last acc)
      in
      let last = len - 1 in
      loop last last empty

(* Searching and selecting elements *)

let left_findi (type t) ?(start = 0) sat v = match length v with
| 0 -> None
| len when start >= len -> None
| len ->
    (* +++ need an iteri_range *)
    let module Sat = struct exception E of (int * t) end in
    let find sat i e =
      if i < start then () else
      if sat i e then raise (Sat.E (i, e)) else ()
    in
    try Pveck.iteri_left (find sat) v; None with
    | Sat.E fnd -> Some fnd

let right_findi (type t) ?start sat v = match length v with
| 0 -> None
| len ->
    (* +++ need an iteri_range *)
    let start = match start with
    | None -> len - 1
    | Some start -> if start >= len then len - 1 else start
    in
    if start < 0 then None else
    let module Sat = struct exception E of (int * t) end in
    let find sat i e =
      if i > start then () else
      if sat i e then raise (Sat.E (i, e)) else ()
    in
    try Pveck.iteri_right (find sat) v; None with
    | Sat.E fnd -> Some fnd

let left_find ?start sat v = left_findi ?start (fun _ e -> sat e) v
let right_find ?start sat v = right_findi ?start (fun _ e -> sat e) v

let partitioni sat v =
  if length v = 0 then empty, empty else
  (* +++ write the loop to avoid tuple. Sharing leaves. Mutate tail. *)
  let f sat (t, f) i el = match sat i el with
  | true -> add_last t el, f
  | false -> t, add_last f el
  in
  foldi_left (f sat) (empty, empty) v

let partition sat v = partitioni (fun _ e -> sat e) v

let filteri sat v =
  if length v = 0 then empty else
  (* +++ sharing leaves whenever sat for all leaf element. Mutate tail *)
  let f sat acc i el = if sat i el then add_last acc el else acc in
  foldi_left (f sat) empty v

let filter sat v = filteri (fun _ e -> sat e) v

(* Edit distance *)

let edit_distance ~eq v0 v1 =
  (* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
  let minimum a b c = min a (min b c) in
  let m = length v0 in
  let n = length v1 in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let el i j = if i = 0 then j else if j = 0 then i else 0 in
  let d = Pveck.init ~len:(m + 1) (fun i -> init ~len:(n + 1) (el i)) in
  let get_el d i j = Pveck.get (Pveck.get d i) j in
  let mset_el d i j e = Pveck.mset (Pveck.get d i) j e in
  for j = 1 to n do
    for i = 1 to m do
      match eq (Pveck.get v0 (i - 1)) (Pveck.get v1 (j - 1)) with
      | true -> mset_el d i j (get_el d (i-1) (j-1))  (* noop required *)
      | false ->
          mset_el d i j @@
          minimum
            (get_el d (i-1) (j) + 1)   (* a deletion *)
            (get_el d (i) (j-1) + 1)   (* an insertion *)
            (get_el d (i-1) (j-1) + 1) (* a substitution *)
    done;
  done;
  get_el d m n

let suggest ?(dist = 2) ~eq candidates v =
  let add (min, acc) name =
    let d = edit_distance ~eq v name in
    if d = min then min, add_last acc name else
    if d < min then d, singleton name else
    min, acc
  in
  let d, suggs = fold_left add (max_int, empty) candidates in
  if d <= dist (* suggest only if not too far *) then suggs else empty

(* Conversions *)

let of_list l =
  (* +++ this a bit ugly, could devise a streaming perfect_tree that
     builds bottom up. *)
  let len = List.length l in
  let l = ref l in
  let el i = let e = List.hd !l in l := List.tl !l; e in
  let mk_leaf ~zero ~len = Pveck.Leaf.init ~len el in
  Pveck.init_leaves ~len mk_leaf

let to_list v = fold_right List.cons v []

let of_array a =
  let of_sub_array a ~zero ~len = Pveck.Leaf.of_sub_array a zero ~len in
  Pveck.init_leaves ~len:(Array.length a) (of_sub_array a)

let to_array v = match length v with
| 0 -> [||]
| len when len > Sys.max_array_length -> err_array len
| len ->
    let a = Array.make len (get_first v) in
    let blit a fst l = Pveck.Leaf.blit_array l a fst (Pveck.Leaf.length l) in
    Pveck.iteri_left_leaves (blit a) v;
    a

let of_bytes b =
  let of_sub_bytes b ~zero ~len =
    Pveck.Leaf.init ~len (fun i -> Bytes.get b (zero + i))
  in
  Pveck.init_leaves ~len:(Bytes.length b) (of_sub_bytes b)

let to_bytes v = match length v with
| 0 -> Bytes.empty
| len when len > Sys.max_string_length -> err_string len
| len ->
    let b = Bytes.create len in
    let to_bytes b fst l =
      for i = 0 to Pveck.Leaf.length l - 1 do
        Bytes.set b (fst+i) (Pveck.Leaf.get l i)
      done
    in
    Pveck.iteri_left_leaves (to_bytes b) v; b

let of_string s =
  let of_sub_string s ~zero ~len =
    Pveck.Leaf.init ~len (fun i -> String.get s (zero + i))
  in
  Pveck.init_leaves ~len:(String.length s) (of_sub_string s)

let to_string v = Bytes.unsafe_to_string @@ to_bytes v

(* Pretty printing *)

let pp ?sep:(pp_sep = Format.pp_print_cut) pp_v ppf v =
  let fst = ref true in
  let pp_elt _ v = if !fst then (fst := false) else pp_sep ppf (); pp_v ppf v in
  iteri_left pp_elt v

let pp_chars ppf v = iter_left (Format.pp_print_char ppf) v

let dump pp_v ppf v =
  Format.fprintf ppf "@[<1>(%a)@]" (pp ~sep:Format.pp_print_space pp_v) v

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The pvec programmers

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
