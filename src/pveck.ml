(*---------------------------------------------------------------------------
   Copyright (c) 2016 The pvec programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Pvec's kernel operations on which all other are expressed.
   Note that these operations do not perform bound checks. *)

let array_iter_right f a =
  for i = Array.length a - 1 downto 0 do f (Array.get a i) done

(* Tree leaves *)

module Leaf = struct
  type 'a t = 'a array
  let v ~len e = Array.make len e
  let init ~len f = Array.init len f
  let singleton v = [|v|] [@@ ocaml.inline]
  let length = Array.length
  let copy = Array.copy
  let get = Array.get
  let pset l i v = let c = Array.copy l in Array.set c i v; c [@@ ocaml.inline]
  let mset = Array.set
  let mblit ~src ss ~dst ds ~len = Array.blit src ss dst ds len
  let iter_left = Array.iter
  let iter_right = array_iter_right
  let fold_left = Array.fold_left
  let fold_right = Array.fold_right
  let map = Array.map
  let of_sub_array a start ~len = Array.sub a start len [@@ ocaml.inline]
  let blit_array l a start ~len = Array.blit l 0 a start len
end

(* Tree nodes *)

module Node = struct
  let create len f = Array.init len f [@@ ocaml.inline]
  let singleton n = Array.make 1 n
  let doubleton n0 n1 = [|n0; n1|]
  let length = Array.length
  let get = Array.get
  let pset n i v = let c = Array.copy n in Array.set c i v; c [@@ ocaml.inline]
  let add_last n c =
    let n' = Array.make (Array.length n + 1) c in
    Array.blit n 0 n' 0 (Array.length n); n'

  let fold_left = Array.fold_left
  let fold_right = Array.fold_right
  let iter_left = Array.iter
  let iter_right = array_iter_right
end

(* Radix *)

let ipow x y =
  let rec loop r x = function 0 -> r | y -> loop (r * x) x (y - 1) in loop 1 x y

let ilog2 x =
  let rec loop p = function 0 -> p | x -> loop (p + 1) (x lsr 1) in loop (-1) x

let branching_factor = 32 (* can be adjusted to use another power of 2. *)
let m = branching_factor

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

(* Vectors *)

type 'a tree =
| Node of 'a tree array
| Leaf of 'a Leaf.t
| Empty

let rec new_branch leaf = function (* for a given shift, i.e. depth *)
| 0 -> Leaf leaf
| s -> Node (Node.singleton (new_branch leaf (next_shift s)))

type 'a t = { shift : int; len : int; tree : 'a tree; }

let length v = v.len [@@ ocaml.inline]

let tree_is_full v = v.len = capacity_of_shift v.shift [@@ ocaml.inline]
let tree_tail_first v = (v.len - 1) land (lnot imax) [@@ ocaml.inline]
let tree_tail_len v = if v.len = 0 then 0 else (v.len - 1 land imax) + 1
[@@ ocaml.inline]

let empty = { shift = 0; len = 0; tree = Empty }

let init_leaves ~len mk_leaf = (* perfect tree *)
  let rec tree cap len i = match cap with
  | cap when cap = m -> Leaf (mk_leaf ~zero:i ~len)
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

let v ~len e = init_leaves ~len (fun ~zero:_ ~len -> Leaf.v ~len e)
let init ~len el =
  let mk_leaf ~zero ~len = Leaf.init ~len (fun i -> el (zero + i)) in
  init_leaves ~len mk_leaf

let singleton v = { shift = 0; len = 1; tree = Leaf (Leaf.singleton v) }
[@@ ocaml.inline]

let copy v =
  let rec copy = function
  | Leaf l -> Leaf (Leaf.copy l)
  | Node n -> Node (Node.create (Node.length n) (fun i -> copy (Node.get n i)))
  | Empty -> Empty
  in
  { v with tree = copy v.tree }

let get v i =
  let rec get_first = function
  | Leaf l -> Leaf.get l 0
  | Node n -> get_first (Node.get n 0)
  | Empty -> assert false
  in
  let rec get_last = function
  | Leaf l -> Leaf.get l (Leaf.length l - 1)
  | Node n -> get_last (Node.get n (Node.length n - 1))
  | Empty -> assert false
  in
  let rec get i s = function
  | Leaf l -> Leaf.get l (i land imax)
  | Node n -> get i (next_shift s) (Node.get n ((i lsr s) land imax))
  | Empty -> assert false
  in
  if i = 0 then get_first v.tree else
  if i = v.len - 1 then get_last v.tree else
  (* +++ unclear whether the above two cases are useful. Measure. *)
  get i v.shift v.tree

let set v i e =
  let rec set i e s = function
  | Leaf l -> Leaf (Leaf.pset l (i land imax) e)
  | Node n ->
      let k = (i lsr s) land imax in
      Node (Node.pset n k (set i e (next_shift s) (Node.get n k)))
  | Empty -> assert false
  in
  { v with tree = set i e v.shift v.tree }

let mset v i e =
  let rec mset i e s = function
  | Leaf l -> Leaf.mset l (i land imax) e
  | Node n -> mset i e (next_shift s) (Node.get n ((i lsr s) land imax))
  | Empty -> assert false
  in
  mset i e v.shift v.tree

let add_last v e =
  let rec add i e s = function
  | Leaf l ->
      let l' = Leaf.v (Leaf.length l + 1) e in
      Leaf.mblit ~src:l 0 ~dst:l' 0 ~len:(Leaf.length l);
      Leaf l'
  | Node n ->
      let ci = (i lsr s) land imax in
      if ci < Node.length n
      then Node (Node.pset n ci (add i e (next_shift s) (Node.get n ci)))
      else
      Node (Node.add_last n (new_branch (Leaf.singleton e) (next_shift s)))
  | Empty -> Leaf (Leaf.singleton e)
  in
  match tree_is_full v with
  | true ->
      let branch = new_branch (Leaf.singleton e) v.shift in
      let shift = prev_shift v.shift in
      { shift; len = v.len + 1; tree = Node (Node.doubleton v.tree branch) }
  | false ->
      { v with len = v.len + 1; tree = add v.len e v.shift v.tree }

let append v0 v1 = match length v0, length v1 with (* TODO *)
| 0, _ -> v1
| _, 0 -> v0
| v0_len, v1_len ->
    (* +++ ridiculous *)
    let el v0_len i = match i with
    | i when i < v0_len -> get v0 i
    | i -> get v1  (i - v0_len)
    in
    init ~len:(v0_len + v1_len) (el v0_len)

let add_first e v = match v.len with (* TODO *)
| 0 -> singleton e
| len -> append (singleton e) v

let range ~first ~last v = (* TODO *)
  let rec loop last acc i = match i > last with
  | true -> acc
  | false -> loop last (add_last acc (get v i)) (i + 1)
  in
  loop last empty first

let break ~left_len v = (* TODO *)
  range ~first:0 ~last:(left_len - 1) v,
  range ~first:left_len ~last:(length v - 1) v

let iter_left f v =
  let rec iter_left f = function
  | Leaf l -> Leaf.iter_left f l
  | Node n -> Node.iter_left (iter_left f) n
  | Empty -> ()
  in
  iter_left f v.tree

let iter_right f v =
  let rec iter_right f = function
  | Leaf l -> Leaf.iter_right f l
  | Node n -> Node.iter_right (iter_right f) n
  | Empty -> ()
  in
  iter_right f v.tree

let iteri_left f v =
  let rec iteri_left f fst = function
  | Leaf l ->
      let len = Leaf.length l in
      for i = 0 to len - 1 do f (fst + i) (Leaf.get l i) done; fst + len
  | Node n -> Node.fold_left (iteri_left f) fst n
  | Empty -> -1 (* ignored *)
  in
  ignore (iteri_left f 0 v.tree)

let iteri_left_leaves f v =
  let rec iteri_left_leaves f fst = function
  | Leaf l -> f fst l; fst + Leaf.length l
  | Node n -> Node.fold_left (iteri_left_leaves f) fst n
  | Empty -> -1 (* ignored *)
  in
  ignore (iteri_left_leaves f 0 v.tree)

let iteri_right f v =
  let rec iteri_right f t lst = match t with
  | Leaf l ->
      let len = Leaf.length l in
      for i = 0 to len - 1 do f (lst - i) (Leaf.get l (len - i - 1)) done;
      lst - len
  | Node n -> Node.fold_right (iteri_right f) n lst
  | Empty -> -1 (* ignored *)
  in
  ignore (iteri_right f v.tree (v.len - 1))

let fold_left f acc v =
  let rec fold_left f acc = function
  | Leaf l -> Leaf.fold_left f acc l
  | Node n -> Node.fold_left (fold_left f) acc n
  | Empty -> acc
  in
  fold_left f acc v.tree

let fold_right f v acc =
  let rec fold_right f v acc = match v with
  | Leaf l -> Leaf.fold_right f l acc
  | Node n -> Node.fold_right (fold_right f) n acc
  | Empty -> acc
  in
  fold_right f v.tree acc

let rec map f v =
  let rec map f = function
  | Leaf l -> Leaf (Leaf.map f l)
  | Node n -> Node (Node.create (Node.length n) (fun i -> map f (Node.get n i)))
  | Empty -> Empty
  in
  {v with tree = map f v.tree }

let equal ~eq v0 v1 = (* assert (length v0 = length v1) *)
  let rec loop eq t0 t1 = match t0, t1 with
  | Leaf l0, Leaf l1 ->
      let rec equal_els eq last l0 l1 i =
        i > last ||
        (eq (Leaf.get l0 i) (Leaf.get l1 i) &&
         equal_els eq last l0 l1 (i + 1))
      in
      if l0 == l1 then true else equal_els eq (Leaf.length l0 - 1) l0 l1 0
  | Node n0, Node n1 ->
      let rec equal_children eq last n0 n1 i =
        i > last ||
        (loop eq (Node.get n0 i) (Node.get n1 i) &&
         equal_children eq last n0 n1 (i + 1))
      in
      if n0 == n1 then true else
      equal_children eq (Node.length n0 - 1) n0 n1 0
  | Empty, Empty -> true
    | _, _ -> assert false
  in
  loop eq v0.tree v1.tree

let compare ~cmp v0 v1 = (* assert (length v0 = length v1) *)
  let rec loop cmp t0 t1 = match t0, t1 with
  | Leaf l0, Leaf l1 ->
      let rec compare_els cmp last l0 l1 i = match i > last with
      | true -> 0
      | false ->
          let c = cmp (Leaf.get l0 i) (Leaf.get l1 i) in
          if c <> 0 then c else compare_els cmp last l0 l1 (i + 1)
      in
      if l0 == l1 then 0 else compare_els cmp (Leaf.length l0 - 1) l0 l1 0
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
