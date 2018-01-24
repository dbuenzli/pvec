(*---------------------------------------------------------------------------
   Copyright (c) 2016 The pvec programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let ipow x y =
  let rec loop r x = function 0 -> r | y -> loop (r * x) x (y - 1) in loop 1 x y

let ilog2 x =
  let rec loop p = function 0 -> p | x -> loop (p + 1) (x lsr 1) in loop (-1) x

let m = 32 (* Branching factor, can be adjusted to use another power of 2. *)
let branching = m (* for the API *)

let imax = m - 1
let log2_m = ilog2 m
let max_depth = (Sys.int_size - 1) / log2_m - 1
let max_length = ipow m (max_depth + 1)

let shift_of_depth d = d * log2_m [@@ ocaml.inline]
let depth_of_shift s = s / log2_m [@@ ocaml.inline]
let next_shift s = s - log2_m [@@ ocaml.inline]
let prev_shift s = s + log2_m [@@ ocaml.inline]
let capacity_of_shift s = m lsl s [@@ ocaml.inline]
let capacity_and_shift_for_len len =
  let rec loop len m_pow depth = match len <= m_pow with
  | false -> loop len (m_pow * m) (depth + 1)
  | true -> m_pow, shift_of_depth (depth - 1)
  in
  loop len m 1

(* Errors *)

let invalidf fmt = Printf.ksprintf (fun s -> invalid_arg s) fmt
let err_length l = invalidf "length %d not in [0;%d]" l max_length
let err_index i = function
| 0 -> invalidf "indexing empty vector with %d" i
| len -> invalidf "index %d not in [0;%d]" i (len - 1)

let err_neg i = invalidf "negative index (%d)" i
let err_empty () = invalidf "empty vector"
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

(* Array functions *)

let array_iter_right f a =
  for i = Array.length a - 1 downto 0 do f (Array.get a i) done

(* Leaves *)

module Leaf = struct
  type 'a t = 'a array

  let v ~len e = Array.make len e
  let init f ~len fst = Array.init len (fun i -> f (fst + i)) [@@ ocaml.inline]
  let singleton v = [|v|] [@@ ocaml.inline]
  let copy = Array.copy
  let length = Array.length
  let get = Array.get
  let pset l i v = let c = Array.copy l in Array.set c i v; c [@@ ocaml.inline]
  let mset = Array.set

  let equal_els ~eq l0 l1 = (* assert (length l0 = length l1) *)
    let rec loop eq last l0 l1 i = match i > last with
    | true -> true
    | false -> eq (get l0 i) (get l1 i) && loop eq last l0 l1 (i + 1)
    in
    if l0 == l1 then true else loop eq (length l0 - 1) l0 l1 0

  let compare_els ~cmp l0 l1 = (* assert (length l0 = length l1) *)
    let rec loop cmp last l0 l1 i = match i > last with
    | true -> 0
    | false ->
        let c = cmp (get l0 i) (get l1 i) in
        if c <> 0 then c else loop cmp last l0 l1 (i + 1)
    in
    if l0 == l1 then 0 else loop cmp (length l0 - 1) l0 l1 0

  let rev l =
    let l = Array.copy l in
    let last = Array.length l - 1 in
    for i = 0 to last / 2 do
      let j = last - i in
      let t = l.(i) in
      l.(i) <- l.(j); l.(j) <- t
    done;
    l

  let fold_left = Array.fold_left
  let fold_right = Array.fold_right

  let mapi f fst l = Array.mapi (fun i e -> f (fst + i) e) l
  let map f l = Array.map f l
  let iteri_left f fst l = Array.iteri (fun i e -> f (fst + i) e) l
  let iteri_right f lst l =
    let len = length l in
    let fst = lst - len + 1 in
    for i = lst downto fst do f i (get l (i - fst)) done

  let iter_left = Array.iter
  let iter_right = array_iter_right
  let exists = Array.exists
  let for_all = Array.for_all

  let left_find sat fst l ~start =
    let rec loop sat fst l last i = match i > last with
    | true -> None
    | false ->
        let e = get l i in
        if sat e then (Some (fst + i, e)) else loop sat fst l last (i + 1)
    in
    loop sat fst l (length l - 1) start

  let right_find sat l lst ~start =
    let rec loop sat l lst i = match i < 0 with
    | true -> None
    | false ->
        let e = get l i in
        if sat e then (Some (lst - i, e)) else loop sat l lst (i - 1)
    in
    loop sat l lst start

  exception Index of int

  let find_false_index_right sat lst l =
    let rec loop sat lst l i = match i < 0 with
    | true -> lst - (length l)
    | false ->
        if sat (get l i) then loop sat lst l (i - 1) else
        raise_notrace (Index (lst - i))
    in
    loop sat lst l (length l - 1)

  let of_sub_array a ~len i = Array.sub a i len [@@ ocaml.inline]
  let of_sub_string s ~len i = init (String.get s) len i
  let of_sub_bytes s ~len i = init (Bytes.get s) len i
  let blit_to_array a i l = Array.blit l 0 a i (length l); i + length l
  let blit_to_bytes b i l = iteri_left (Bytes.set b) i l; i + length l

  let add_last l v =
    let l' = Array.make (Array.length l + 1) v in
    Array.blit l 0 l' 0 (Array.length l); l'

  let find_unsat_left ret sat fst ~start l =
    let rec loop ret sat k l last i = match i > last with
    | true -> k + 1
    | false ->
        let e = get l i in
        if sat k e then loop ret sat (k + 1) l last (i + 1) else
        ret k e
    in
    loop ret sat (fst + start) l (length l - 1) start

  let find_unsat_right ret sat lst ~start l =
    let rec loop ret sat k l i = match i < 0 with
    | true -> k - 1
    | false ->
        let e = get l i in
        if sat k e then loop ret sat (k - 1) l (i - 1) else
        ret k e
    in
    loop ret sat (lst - start) l start
end

(* Tree nodes *)

module Node = struct
  let create len f = Array.init len f [@@ ocaml.inline]
  let singleton n = Array.make 1 n
  let doubleton n0 n1 = [|n0; n1|]
  let length = Array.length
  let get = Array.get
  let pset n i v = let c = Array.copy n in Array.set c i v; c [@@ ocaml.inline]
  let fold_left = Array.fold_left
  let fold_right = Array.fold_right
  let iter_left = Array.iter
  let iter_right = array_iter_right
  let for_all = Array.for_all
  let exists = Array.exists
  let add_last n c =
    let n' = Array.make (Array.length n + 1) c in
    Array.blit n 0 n' 0 (Array.length n); n'
end

(* Trees *)

type 'a tree =
| Node of 'a tree array
| Leaf of 'a Leaf.t
| Empty

module Tree = struct

  let rec branch leaf = function (* for a given shift, i.e. depth *)
  | 0 -> Leaf leaf
  | s -> Node (Node.singleton (branch leaf (next_shift s)))

  (* These functions act over the leaves as chunks. *)

  let rec fold_left_leaves f acc = function
  | Leaf l -> f acc l
  | Node n -> Node.fold_left (fold_left_leaves f) acc n
  | Empty -> acc

  let rec fold_right_leaves f t acc = match t with
  | Leaf l -> f l acc
  | Node n -> Node.fold_right (fold_right_leaves f) n acc
  | Empty -> acc

  let iteri_left_leaves f t =
    let rec loop f fst = function
    | Leaf l -> f fst l; fst + Leaf.length l
    | Node n -> Node.fold_left (loop f) fst n
    | Empty -> -1 (* ignored *)
    in
    ignore (loop f 0 t)

  let iteri_right_leaves f t len =
    let rec loop f t lst = match t with
    | Leaf l -> f lst l; lst - Leaf.length l
    | Node n -> Node.fold_right (loop f) n lst
    | Empty -> -1 (* ignored *)
    in
    ignore (loop f t (len - 1))

  let rec map_leaves f = function
  | Leaf l -> f l
  | Node n ->
      Node (Node.create (Node.length n) (fun i -> map_leaves f (Node.get n i)))
  | Empty -> Empty

  let copy t = map_leaves (fun l -> Leaf (Leaf.copy l)) t

  (* These functions act over elements *)

  let mapi f t =
    let rec loop f fst = function
    | Leaf l ->
        let n = Leaf (Leaf.mapi f !fst l) in
        fst := !fst + Leaf.length l; n
    | Node n ->
        Node (Node.create (Node.length n) (fun i -> loop f fst (Node.get n i)))
    | Empty -> Empty
    in
    loop f ((* ugh *) ref 0) t

  let rec iter_left f = function
  | Leaf l -> Leaf.iter_left f l
  | Node n -> Node.iter_left (iter_left f) n
  | Empty -> ()

  let rec iter_right f = function
  | Leaf l -> Leaf.iter_right f l
  | Node n -> Node.iter_right (iter_right f) n
  | Empty -> ()

  let rec for_all p = function
  | Leaf l -> Leaf.for_all p l
  | Node n -> Node.for_all (for_all p) n
  | Empty -> false

  let rec exists p = function
  | Leaf l -> Leaf.exists p l
  | Node n -> Node.exists (exists p) n
  | Empty -> false

  let rec get_first = function
  | Leaf l -> Leaf.get l 0
  | Node n -> get_first (Node.get n 0)
  | Empty -> err_empty ()

  let rec get_last = function
  | Leaf l -> Leaf.get l (Leaf.length l - 1)
  | Node n -> get_last (Node.get n (Node.length n - 1))
  | Empty -> err_empty ()

  let rec set_first e = function
  | Leaf l -> Leaf (Leaf.pset l 0 e)
  | Node n -> Node (Node.pset n 0 (set_first e (Node.get n 0)))
  | Empty -> err_empty ()

  let rec set_last e = function
  | Leaf l -> Leaf (Leaf.pset l (Leaf.length l - 1) e)
  | Node n ->
      let last = (Node.length n - 1) in
      Node (Node.pset n last (set_last e (Node.get n last)))
  | Empty -> err_empty ()

  let find_false_index_right sat t len =
    let rec loop sat t lst = match t with
    | Leaf l -> Leaf.find_false_index_right sat lst l
    | Node n -> Node.fold_right (loop sat) n lst
    | Empty -> err_empty ()
    in
    try loop sat t (len - 1) (* returns [-1] *) with Leaf.Index i -> i

  exception Index of int
  let ret_index i _ = raise_notrace (Index i)

  let find_false_index_left sat t =
    let rec loop ret sat fst = function
    | Leaf l -> Leaf.find_unsat_left ret sat fst ~start:0 l
    | Node n -> Node.fold_left (loop ret sat) fst n
    | Empty -> err_empty ()
    in
    let sat _ e = sat e in
    try loop ret_index sat 0 t (* returns len of [t] *) with Index i -> i

  (* The following functions assume non empty [t]. *)

  let rec get i s = function
  | Leaf l -> Leaf.get l (i land imax)
  | Node n -> get i (next_shift s) (Node.get n ((i lsr s) land imax))
  | Empty -> assert false

  let rec set i e s = function
  | Leaf l -> Leaf (Leaf.pset l (i land imax) e)
  | Node n ->
      let k = (i lsr s) land imax in
      Node (Node.pset n k (set i e (next_shift s) (Node.get n k)))
  | Empty -> assert false

  let rec mset i e s = function
  | Leaf l -> Leaf.mset l (i land imax) e
  | Node n -> mset i e (next_shift s) (Node.get n ((i lsr s) land imax))
  | Empty -> assert false

  let rev len s = function
  | Leaf l -> Leaf (Leaf.rev l)
  | Node _ as n ->
      let n = copy n in
      (* +++
         1. If the length is a multiple of m reverse leaves and their content.
         2. [iter_left] and [iter_right] at the same time and swap. *)
      let last = len - 1 in
      for i = 0 to last / 2 do
        let j = last - i in
        let t = get i s n in
        mset i (get j s n) s n; mset j t s n
      done;
      n
  | Empty -> Empty

  let left_find sat ~start s t =
    let rec find_start_leaf sat i s = function
    | Leaf l -> failwith "TODO"
    | Node n ->
        let start_leaf = (i lsr s) land imax in
        let start_child = Node.get n start_leaf in
        begin match find_start_leaf sat i (next_shift s) start_child with
        | Some _ as v -> v
        | None ->
            let rec find_right_children last i = match i > last with
            | true -> None
            | false ->
                match find_leaf sat 0 (Node.get n i) with
                | None -> find_right_children last (i + 1)
                | Some _ as v -> v
            in
            find_right_children (Node.length n) (start_leaf + 1)
        end
    | Empty -> assert false
    and find_leaf sat fst = function
    | Leaf l -> failwith "TODO"
    | Node n -> failwith "TODO"
    | Empty -> assert false
    in
    find_start_leaf sat start s t

  let right_find sat ~start s t = failwith "TODO"
end

(* Vectors *)

type 'a t = { shift : int; len : int; tree : 'a tree; }

let length v = v.len [@@ ocaml.inline]

let tree_is_full v = v.len = capacity_of_shift v.shift [@@ ocaml.inline]
let tree_tail_first v = (v.len - 1) land (lnot imax) [@@ ocaml.inline]
let tree_tail_len v = if v.len = 0 then 0 else (v.len - 1 land imax) + 1
[@@ ocaml.inline]

(* Constructors *)

let copy v = { v with tree = Tree.copy v.tree }
let empty = { shift = 0; len = 0; tree = Empty }
let singleton v = { shift = 0; len = 1; tree = Leaf (Leaf.singleton v) }
[@@ ocaml.inline]

let perfect_tree len mk_leaf =
  let rec tree cap len i = match cap with
  | cap when cap = m -> Leaf (mk_leaf ~len i)
  | cap ->
      let child_cap = cap / m in
      let child_count, last_child_len = match len mod child_cap with
      | 0 -> len / child_cap, child_cap
      | r -> len / child_cap + 1, r
      in
      let child k =
        let i = i + k * child_cap in
        let len = if k = child_count - 1 then last_child_len else child_cap in
        tree (cap / m) len i
      in
      Node (Node.create child_count child)
  in
  if len = 0 then empty else
  let cap, shift = capacity_and_shift_for_len len in
  { shift; len; tree = tree cap len 0 }

let v ~len e = match 0 <= len && len <= max_length with
| false -> err_length len
| true -> perfect_tree len (fun ~len _fst -> Leaf.v len e)

let init ~len f = match 0 <= len && len <= max_length with
| false -> err_length len
| true -> perfect_tree len (Leaf.init f)

let ints ?(start = 0) n = match start > n with
| true -> empty
| false -> init ~len:(n + 1 - start) (fun i -> start + i)

let add_last v e =
  let rec push i e s = function
  | Leaf l -> Leaf (Leaf.add_last l e)
  | Node n ->
      let ci = (i lsr s) land imax in
      if ci < Node.length n
      then Node (Node.pset n ci (push i e (next_shift s) (Node.get n ci)))
      else
      Node (Node.add_last n (Tree.branch (Leaf.singleton e) (next_shift s)))
  | Empty -> Leaf (Leaf.singleton e)
  in
  let len = v.len + 1 in
  match len > max_length with
  | true -> err_max_length ()
  | false ->
      match tree_is_full v with
      | true ->
          let branch = Tree.branch (Leaf.singleton e) v.shift in
          let shift = prev_shift v.shift in
          { shift; len; tree = Node (Node.doubleton v.tree branch) }
      | false ->
          { v with len; tree = push v.len e v.shift v.tree }

let _range ~first ~last v = (* assumes ~first ~last v in bounds *)
  (* +++ ridiculous. Reuse leaves, in particular when [first]
     starts on a leaf chunk (e.g. 0), important for other ops (e.g. [take_*]) *)
  let rec loop last acc i = match i > last with
  | true -> acc
  | false -> loop last (add_last acc (Tree.get i v.shift v.tree)) (i + 1)
  in
  loop last empty first

let _break n v = (* assumes n in [0; length v] *)
  (* +++ This should likely not be defined in terms of _range *)
  _range ~first:0 ~last:(n - 1) v,
  _range ~first:n ~last:(length v - 1) v

(* Predicates and comparisons *)

let indexed_pred_left p = let i = ref (-1) in fun e -> incr i; p !i e
let indexed_pred_right p len = let i = ref len in fun e -> decr i; p !i e

let is_empty v = length v = 0
let is_filled v i = 0 <= i && i <= length v - 1
let is_prefix ~eq ~affix v =
  (* +++ do this over the leaves directly rather than [get]ing *)
  let len_a = length affix in
  let len_v = length v in
  if len_a > len_v then false else
  let rec loop affix v last i = match i > last with
  | true -> true
  | false ->
      let ai = Tree.get i affix.shift affix.tree in
      let vi = Tree.get i v.shift v.tree in
      eq ai vi && loop affix v last (i + 1)
  in
  loop affix v (len_a - 1) 0

let is_infix ~eq ~affix v =
  (* +++ do this over the leaves directly rather than [get]ing *)
  let len_a = length affix in
  let len_v = length v in
  if len_a > len_v then false else
  let rec loop affix v last_a max_v i k = match i > max_v with
  | true -> false
  | false ->
      match k > last_a with
      | true -> true
      | false ->
          let ai = Tree.get k affix.shift affix.tree in
          let vi = Tree.get (i + k) v.shift v.tree in
          if eq ai vi
          then loop affix v last_a max_v i (k + 1)
          else loop affix v last_a max_v (i + 1) 0
  in
  let last_a = len_a - 1 in
  let max_v = len_v - len_a in
  loop affix v last_a max_v 0 0

let is_suffix ~eq ~affix v =
  (* +++ do this over the leaves directly rather than [get]ing *)
  let last_a = length affix - 1 in
  let last_v = length v - 1 in
  if last_a > last_v then false else
  let rec loop affix v last_a last_v i = match i > last_a with
  | true -> true
  | false ->
      let ai = Tree.get (last_a - i) affix.shift affix.tree in
      let vi = Tree.get (last_v - i) v.shift v.tree in
      eq ai vi && loop affix v last_a last_v (i + 1)
  in
  loop affix v last_a last_v 0

let for_all p v = Tree.for_all p v.tree
let exists p v = Tree.exists p v.tree

let for_alli p v = for_all (indexed_pred_left p) v
let existsi p v = exists (indexed_pred_left p) v

let equal ~eq v0 v1 = match length v0 = length v1 with
| false -> false
| true ->
    let rec loop eq t0 t1 = match t0, t1 with
    | Leaf l0, Leaf l1 -> Leaf.equal_els ~eq l0 l1
    | Node n0, Node n1 ->
        let rec equal_children eq last n0 n1 i = match i > last with
        | true -> true
        | false ->
            loop eq (Node.get n0 i) (Node.get n1 i) &&
            equal_children eq last n0 n1 (i + 1)
        in
        if n0 == n1 then true else
        equal_children eq (Node.length n0 - 1) n0 n1 0
    | Empty, Empty -> true
    | _, _ -> assert false
    in
    loop eq v0.tree v1.tree

let compare ~cmp v0 v1 = match compare (length v0) (length v1) with
| (-1 | 1) as i -> i
| 0 ->
    let rec loop cmp t0 t1 = match t0, t1 with
    | Leaf l0, Leaf l1 -> Leaf.compare_els ~cmp l0 l1
    | Node n0, Node n1 ->
        let rec compare_children cmp last n0 n1 i = match i > last with
        | true -> 0
        | false ->
            let c = loop cmp (Node.get n0 i) (Node.get n1 i) in
            if c <> 0 then c else compare_children cmp last n0 n1 (i + 1)
        in
        if n0 == n1 then 0 else
        compare_children compare (Node.length n0 - 1) n0 n1 0
    | Empty, Empty -> 0
    | _, _ -> assert false
    in
    loop cmp v0.tree v1.tree
| _ -> assert false

(* Getting elements *)

let get v i = match 0 <= i && i < length v with
| true -> Tree.get i v.shift v.tree
| false -> err_index i v.len

let get_first v = Tree.get_first v.tree
let get_last v = Tree.get_last v.tree

let el v i =
  if 0 <= i && i < length v then Some (Tree.get i v.shift v.tree) else None

let first_el v = if length v = 0 then None else Some (Tree.get_first v.tree)
let last_el v = if length v = 0 then None else Some (Tree.get_last v.tree)

let range ~first ~last v =
  let len = length v in
  match first > last || first >= len || last < 0 with
  | true -> empty
  | false ->
      let first = if first < 0 then 0 else first in
      let last = if last >= len then len - 1 else last in
      _range ~first ~last v

(* Adding *)

let append v0 v1 = match length v0, length v1 with
| 0, _ -> v1
| _, 0 -> v0
| v0_len, v1_len ->
    (* +++ ridiculous *)
    let el v0_len i = match i with
    | i when i < v0_len -> get v0 i
    | i -> get v1  (i - v0_len)
    in
    init ~len:(v0_len + v1_len) (el v0_len)

let ( ++ ) = append

let add_first e u = match length u with
| 0 -> singleton e
| len -> singleton e ++ u

let concat ?(sep = empty) vs = match length vs with
| 0 -> empty
| _ ->
    (* +++ something better than this can be done e.g. with a streaming
         perfect balance *)
      let add acc v = acc ++ sep ++ v in
      let first = get_first vs in
      let t = _range ~first:1 ~last:(length vs - 1) vs in
      Tree.fold_left_leaves (Leaf.fold_left add) first t.tree

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
      | false -> _range ~first:0 ~last:(first - 1) into
      in
      let r = match last + 1 > into_last with
      | true -> empty
      | false -> _range ~first:(last + 1) ~last:into_last into
      in
      l ++ v ++ r
  | false ->
      if first < 0 then v ++ into else
      if first > into_last then into ++ v else
      let left, right = _break first into in
      left ++ v ++ right

(* Setting elements *)

let set v i e = match 0 <= i && i < v.len with
| true -> { v with tree = Tree.set i e v.shift v.tree }
| false -> err_index i v.len

let mset v i e = match 0 <= i && i < v.len with
| true -> Tree.mset i e v.shift v.tree
| false -> err_index i v.len

let set_first v e = { v with tree = Tree.set_first e v.tree }
let set_last v e = { v with tree = Tree.set_last e v.tree }

let fill ~pad u i e = match i < 0 with
| true -> err_neg i
| false ->
    match 0 <= i && i < u.len with
    | true -> { u with tree = Tree.set i e u.shift u.tree }
    | false ->
        let i = i - length u in
        let adds = v ~len:(i + 1) pad in
        mset adds i e;
        u ++ adds

let fill_first v e = match length v with
| 0 -> singleton e
| _ -> { v with tree = Tree.set_first e v.tree }

let fill_last v e = match length v with
| 0 -> singleton e
| _ -> { v with tree = Tree.set_last e v.tree }

(* Removing *)

let rem_range ~first ~last v = match first <= last with
| false -> v
| true ->
    let last_v = length v  - 1 in
    if last < 0 then v else
    if first > last_v then v else
    if first <= 0 then _range ~first:(last + 1) ~last:last_v v else
    if last >= last_v then _range ~first:0 ~last:(first - 1) v else
    _range ~first:0 ~last:(first - 1) v ++
    _range ~first:(last + 1) ~last:last_v v

let rem v i = rem_range ~first:i ~last:i v
let rem_first v = rem v 0
let rem_last v = rem v (length v - 1)

(* Traversing *)

let fold_left f acc v = Tree.fold_left_leaves (Leaf.fold_left f) acc v.tree
let fold_right f v acc = Tree.fold_right_leaves (Leaf.fold_right f) v.tree acc

let foldi_left f acc v =
  let f = let i = ref (-1) in fun acc e -> incr i; f acc !i e in
  fold_left f acc v

let foldi_right f v acc =
  let f = let i = ref (length v) in fun e acc -> decr i; f !i e acc in
  fold_right f v acc

let iteri_left f v = Tree.iteri_left_leaves (Leaf.iteri_left f) v.tree
let iter_left f v = Tree.iter_left f v.tree
let iteri_right f v = Tree.iteri_right_leaves (Leaf.iteri_right f) v.tree v.len
let iter_right f v = Tree.iter_right f v.tree

(* Mapping and rearranging elements *)

let mapi f v = { v with tree = Tree.mapi f v.tree }
let map f v =
  let map_leaf l = Leaf (Leaf.map f l) in
  { v with tree = Tree.map_leaves map_leaf v.tree }

let filter_map f v = match length v with
| 0 -> empty
| _ ->
    let fmap f acc e = match f e with
    | None -> acc
    | Some v ->  add_last acc v
    in
    Tree.fold_left_leaves (Leaf.fold_left (fmap f)) empty v.tree

let filter_mapi f v = match length v with
| 0 -> empty
| _ ->
    (* +++ ugly *)
    let i = ref (-1) in
    filter_map (fun e -> incr i; f !i e) v

let rev v = { v with tree = Tree.rev v.len v.shift v.tree }
let indices v = mapi (fun i _ -> i) v

let transpose v = failwith "TODO"
let stable_sort ~cmp v = failwith "TODO"
let sort_uniq ~cmp v = failwith "TODO"

let unstutter ~eq v = match length v with
| 0 -> empty
| n ->
    (* +++ write the loop to avoid tuple. Sharing leaves (?). Mutate tail. *)
    let f eq (prev, unst as acc) e = match eq prev e with
    | true -> acc
    | false -> (e, add_last unst e)
    in
    let first = Tree.get 0 v.shift v.tree in
    let acc = first, singleton first in
    snd (Tree.fold_left_leaves (Leaf.fold_left (f eq)) acc v.tree)

let shuffle ~rand v = (* Fisher-Yates *)
  let v = copy v in
  let rswap i ei = (* +++ operate on leaves to avoid three walks per swap *)
    let j = rand (i + 1) in
    mset v i (get v j);
    mset v j ei
  in
  iteri_right rswap v; v

(* Breaking with magnitudes *)

let take_left n v = range ~first:0 ~last:(n - 1) v
let take_right n v = range ~first:(length v - n) ~last:(length v - 1) v

let drop_left n v = range ~first:n ~last:(length v - 1) v
let drop_right n v = range ~first:0 ~last:(length v - n - 1) v

let break_left n v = match n with
| n when n < 0 -> empty, v
| n when n >= length v -> v, empty
| n -> _break n v

let break_right n v = match n with
| n when n < 0 -> v, empty
| n when n >= length v -> empty, v
| n -> _break (length v - n) v

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
  perfect_tree chunk_count (Leaf.init el)

let chunk_left size v = chunk ~first_is_small:false size v
let chunk_right size v = chunk ~first_is_small:true size v

let pop_first v = match length v with
| 0 -> None
| _ -> Some (Tree.get_first v.tree, drop_left 1 v)

let pop_last v = match length v with
| 0 -> None
| _ -> Some (drop_right 1 v, Tree.get_last v.tree)

(* Breaking with seperators *)

let keep_left sat v = match length v with
| 0 -> empty
| len ->
    let last = len - 1 in
    match Tree.find_false_index_left sat v.tree with
    | 0 -> empty
    | i when i > last -> v
    | i -> _range ~first:0 ~last:(i - 1) v

let keep_right sat v = match length v with
| 0 -> empty
| len ->
    let last = len - 1 in
    match Tree.find_false_index_right sat v.tree v.len with
    | i when i = last -> empty
    | i when i < 0 -> v
    | i -> _range ~first:(i + 1) ~last v

let lose_left sat v = match length v with
| 0 -> empty
| len ->
    let last = len - 1 in
    match Tree.find_false_index_left sat v.tree with
    | 0 -> v
    | i when i > last -> empty
    | i -> _range ~first:i ~last v

let lose_right sat v = match length v with
| 0 -> empty
| len ->
    let last = len - 1 in
    match Tree.find_false_index_right sat v.tree v.len with
    | i when i = last -> v
    | i when i < 0 -> empty
    | i -> _range ~first:0 ~last:i v

let span_left sat v = match length v with
| 0 -> empty, empty
| len ->
    let last = len - 1 in
    match Tree.find_false_index_left sat v.tree with
    | 0 -> empty, v
    | i when i > last -> v, empty
    | i -> _break i v

let span_right sat v = match length v with
| 0 -> empty, empty
| len ->
    let last = len - 1 in
    match Tree.find_false_index_right sat v.tree v.len with
    | i when i = last -> v, empty
    | i when i < 0 -> empty, v
    | i -> _break (i + 1) v

let trim drop v = match length v with
| 0 -> empty
| len ->
    let last = len - 1 in
    match Tree.find_false_index_left drop v.tree with
    | i when i > last -> empty
    | first ->
        match Tree.find_false_index_right drop v.tree v.len with
     (* | i when i < 0 -> assert false *)
        | i when i = last -> if first = 0 then v else _range ~first ~last v
        | i -> _range ~first ~last v

let keepi_left p v = keep_left (indexed_pred_left p) v
let keepi_right p v = keep_left (indexed_pred_right p (length v)) v
let losei_left p v = lose_left (indexed_pred_left p) v
let losei_right p v = lose_right (indexed_pred_right p (length v)) v
let spani_left p v = span_left (indexed_pred_left p) v
let spani_right p v = span_right (indexed_pred_right p (length v)) v

let trimi drop v = failwith "TODO"

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
        | false -> _range ~first:r_first ~last:v_last v
        in
        Some (_range ~first:0 ~last:i v, right)
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
        | false -> _range ~first:0 ~last:(i - 1) v
        in
        Some (left, _range ~first:r_start ~last:v_last v)
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
  | false -> add_last acc (_range ~first ~last v)
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
  | false -> add_first (_range ~first ~last v) acc
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
  | false -> add_first (_range ~first ~last v) acc
  in
  match length v with
  | 0 -> empty
  | len ->
      (* +++ express this using iteration rather than [get]'s *)
      let rec loop i last acc = match i < 0 with
      | true -> add ~drop_empty v ~first:i ~last acc
      | false ->
          match is_sep (get v i) with
          | false -> loop (i - 1) last acc
          | true ->
              loop (i - 1) (i - 1) (add ~drop_empty v ~first:(i + 1) ~last acc)
      in
      let last = len - 1 in
      loop last last empty

(* Searching and selecting elements *)

let left_find ?(start = 0) sat v = match length v with
| 0 -> None
| len when start >= len -> None
| len ->
    let start = if start < 0 then 0 else start in
    Tree.left_find sat ~start v.shift v.tree

let right_find ?start sat v = match length v with
| 0 -> None
| len ->
    let start = match start with
    | None -> len - 1
    | Some start -> if start >= len then len - 1 else start
    in
    match start < 0 with
    | true -> None
    | false -> Tree.right_find sat ~start v.shift v.tree

let partition sat v = match length v with
| 0 -> empty, empty
| n ->
    (* +++ write the loop to avoid tuple. Sharing leaves. Mutate tail. *)
    let f sat (t, f) el = match sat el with
    | true -> add_last t el, f
    | false -> t, add_last f el
    in
    Tree.fold_left_leaves (Leaf.fold_left (f sat)) (empty, empty) v.tree

let filter sat v = match length v with
| 0 -> empty
| _ ->
    (* +++ sharing leaves whenever sat for all leaf elemente. Mutate tail *)
    let f sat acc el = if sat el then add_last acc el else acc in
    Tree.fold_left_leaves (Leaf.fold_left (f sat)) empty v.tree

(* Edit distance *)

let edit_distance ~eq v0 v1 =
  (* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
  let minimum a b c = min a (min b c) in
  let m = length v0 in
  let n = length v1 in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let el i j = if i = 0 then j else if j = 0 then i else 0 in
  let d = init ~len:(m + 1) (fun i -> init ~len:(n + 1) (el i)) in
  let get_el d i j = get (get d i) j in
  let mset_el d i j e = mset (get d i) j e in
  for j = 1 to n do
    for i = 1 to m do match eq (get v0 (i - 1)) (get v1 (j - 1)) with
    | true -> mset_el d i j (get_el d (i-1) (j-1))  (* no operation required *)
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
  let mk_leaf ~len i = Leaf.init el ~len i in
  perfect_tree len mk_leaf

let to_list v = fold_right List.cons v []

let of_array a = perfect_tree (Array.length a) (Leaf.of_sub_array a)
let to_array v = match length v with
| 0 -> [||]
| len when len > Sys.max_array_length -> err_array len
| len ->
    let a = Array.make len (get_first v) in
    Tree.iteri_left_leaves (Leaf.blit_to_array a) v.tree; a

let of_bytes b = perfect_tree (Bytes.length b) (Leaf.of_sub_bytes b)
let to_bytes v = match length v with
| 0 -> Bytes.empty
| len when len > Sys.max_string_length -> err_string len
| len ->
    let b = Bytes.create len in
    Tree.iteri_left_leaves (Leaf.blit_to_bytes b) v.tree; b

let of_string s = perfect_tree (String.length s) (Leaf.of_sub_string s)
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
