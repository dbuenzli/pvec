(*---------------------------------------------------------------------------
   Copyright (c) 2016 The pvec programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let log f = Format.printf (f ^^ "@.")

(* Maximal vector length in random tests *)

let len_hint =
  let m = Pvec.branching in
  [ "leaf", m - 1 (* one leaf *);
    "small", m * m + 1; (* depth of 2, ~1000 els *)
    "medium", m * m * m + 1; (* depth of 3, ~32'000 els *)
    "large", m * m * m * m + 1; (* ~ 1 million elements *)
    "huge", m * m * m * m * m * m + 1; (* ~1 billion elements *) ]

(* Random seed & random test repetition count *)

let rseed, repeat_count, max_len  =
  let rseed = ref (Random.self_init (); Random.bits ()) in
  let max_len = ref (List.assoc "medium" len_hint) in
  let set_max_len_hint symb = max_len := List.assoc symb len_hint in
  let repeat_count = ref 10 in
  let args =
    ["--rseed", Arg.Set_int rseed, "Random seed.";
     "--rlen", Arg.Set_int max_len, "max random vector length";
     "--rlen-hint", Arg.Symbol (List.map fst len_hint, set_max_len_hint),
     "max random vector length hint";
     "--repeat", Arg.Set_int repeat_count, "random reptition count"; ]
  in
  let bad s = raise (Arg.Bad (strf "Don't know what to do with '%s'" s)) in
  Arg.parse args bad "test [OPT]...";
  !rseed, !repeat_count, !max_len

(* Randomness *)

let rand = Random.init rseed; Random.int
let rlen max = rand (max + 1)
let nz_rlen max = rand max + 1
let rrange ~vlen =
  let r0 = rand vlen in
  let r1 = rand vlen in
  if r0 > r1 then r1, r0 else r0, r1

(* Sequences of nat and ints *)

let iseq len = Pvec.ints (len - 1)
let iseq_shuffled len = Pvec.shuffle rand (iseq len)
let iseq_sum is =
  let n = Pvec.length is - 1 in
  (n * (n + 1)) / 2

let nats n = Pvec.ints ~start:1 n
let nats_shuffled n = Pvec.shuffle rand (nats n)

(* Testing predicates *)

let eq = ( = )
let assert_invalid_arg f x = try f x; assert false with Invalid_argument _ -> ()

(* Tests *)

let test_base () =
  log "Testing Pvec.{empty,v,init,singleton,get,set}";
  assert_invalid_arg  Pvec.(get empty) 0;
  assert_invalid_arg  Pvec.(set empty 0) 3;
  let zero = Pvec.singleton 0 in
  let one = Pvec.set zero 0 1 in
  assert (Pvec.get zero 0 = 0);
  assert (Pvec.get one 0 = 1);
  assert_invalid_arg (Pvec.get zero) 1;
  assert_invalid_arg (Pvec.get zero) (-1);
  assert_invalid_arg (Pvec.get one) 1;
  assert_invalid_arg (Pvec.get one) (-1);
  assert (Pvec.v ~len:0 0 == Pvec.empty);
  assert (Pvec.init ~len:0 (fun n -> n) == Pvec.empty);
  assert_invalid_arg Pvec.(v ~len:(max_length + 1)) 0;
  assert_invalid_arg Pvec.(init ~len:(max_length + 1)) (fun n -> n);
  for i = 1 to repeat_count do
    let len = rand max_len in
    let vv = Pvec.v ~len 1 in
    let vi = Pvec.init ~len (fun i -> i) in
    assert (Pvec.length vv = len);
    assert (Pvec.length vi = len);
    for i = 0 to len - 1 do
      assert (Pvec.get vv i = 1);
      assert (Pvec.get vi i = i)
    done;
  done;
  for i = 1 to repeat_count do
    let nat_count = rand max_len in
    let nats = nats nat_count in
    let rev_nats = ref nats in
    for i = 1 to nat_count do
      assert (Pvec.get nats (i - 1) = i);
      rev_nats := Pvec.set !rev_nats (nat_count - i) i;
    done;
    for i = 1 to nat_count do
      assert (Pvec.get !rev_nats (i - 1) = nat_count - i + 1)
    done;
    assert_invalid_arg (Pvec.get nats) nat_count;
    assert_invalid_arg (Pvec.set nats nat_count) 0;
  done;
  ()

let test_conversions () =
  log "Testing Pvec.{of,to}_{list,array,string,bytes}";
  let rec llen acc = function 0 -> acc | n -> llen (n - 1 :: acc) (n - 1) in
  let alen len = Array.init len (fun i -> i) in
  let init i = Char.chr (i mod 256) in
  let slen len = String.init len init in
  let blen len = Bytes.init len init in
  assert Pvec.(of_list [] = Pvec.empty);
  assert Pvec.(of_array [||] == Pvec.empty);
  assert Pvec.(of_string "" == Pvec.empty);
  assert Pvec.(of_bytes Bytes.empty == Pvec.empty);
  assert Pvec.(to_list @@ of_list [] = []);
  assert Pvec.(to_array @@ of_array [||] = [||]);
  assert Pvec.(to_string @@ of_string "" = "");
  assert Pvec.(to_bytes @@ of_bytes Bytes.empty = Bytes.empty);
  for i = 1 to repeat_count do
    let len = rlen max_len in
    let ridx = rand len in
    let l = llen [] len in
    let a = alen len in
    let s = slen len in
    let b = blen len in
    let lv = Pvec.of_list l in
    let av = Pvec.of_array a in
    let sv = Pvec.of_string s in
    let bv = Pvec.of_bytes b in
    assert (Pvec.get lv ridx = List.nth l ridx);
    assert (Pvec.get av ridx = Array.get a ridx);
    assert (Pvec.get sv ridx = String.get s ridx);
    assert (Pvec.get bv ridx = Bytes.get b ridx);
    assert Pvec.(to_list lv = l);
    assert Pvec.(to_array @@ of_array a = a);
    assert Pvec.(to_string @@ of_string s = s);
    assert Pvec.(to_bytes @@ of_bytes b = b);
  done;
  ()

let test_length () =
  log "Testing Pvec.max_length (%d) and Pvec.length" Pvec.max_length;
  (* These invariants are in the doc *)
  assert (Pvec.max_length >= Sys.max_string_length);
  assert (Pvec.max_length >= Sys.max_array_length);
  assert (Pvec.length Pvec.empty = 0);
  assert (Pvec.length (Pvec.v ~len:0 (fun n -> n)) == 0);
  assert (Pvec.length (nats 1_000_000) = 1_000_000);
  ()

let test_eq () =
  log "Testing Pvec.eq";
  assert (Pvec.(equal ~eq empty empty));
  for i = 1 to repeat_count do
    let nz_len = nz_rlen max_len in
    let i0 = iseq nz_len in
    let i1 = iseq nz_len in
    let i2 = Pvec.(set i0 (length i0 - 1) 0) in
    let i3 = iseq (nz_len - 1) in
    assert Pvec.(equal ~eq i0 i0);
    assert Pvec.(equal ~eq i0 i1);
    assert Pvec.(equal ~eq i1 i0);
    assert (not @@ Pvec.(equal ~eq i0 i2));
    assert (not @@ Pvec.(equal ~eq i2 i0));
    assert (not @@ Pvec.(equal ~eq i0 i3));
    assert (not @@ Pvec.(equal ~eq i3 i0));
    assert (not @@ Pvec.(equal ~eq i0 empty));
    assert (not @@ Pvec.(equal ~eq empty i0));
  done;
  ()

let test_compare () =
  log "Testing Pvec.compare";
  let cmp = Pervasives.compare in
  assert Pvec.(compare ~cmp empty empty = 0);
  for i = 1 to repeat_count do
    let nz_len = nz_rlen max_len in
    let i0 = iseq nz_len in
    let i1 = iseq nz_len in
    let i2 = Pvec.(set i0 (length i0 - 1) 0) in
    let i3 = iseq (nz_len - 1) in
    assert Pvec.(compare ~cmp i0 i0 = 0);
    assert Pvec.(compare ~cmp i0 i1 = 0);
    assert Pvec.(compare ~cmp i1 i0 = 0);
    assert Pvec.(compare ~cmp i0 i2 = 1);
    assert Pvec.(compare ~cmp i2 i0 = -1);
    assert Pvec.(compare ~cmp i0 i3 = 1);
    assert Pvec.(compare ~cmp i3 i0 = -1);
    assert Pvec.(compare ~cmp i0 empty = 1);
    assert Pvec.(compare ~cmp empty i0 = -1);
  done;
  ()

let test_predicates () =
  log "Testing Pvec.{is_empty,is_filled}";
  assert Pvec.(is_empty empty);
  for i = 1 to repeat_count do
    let is = iseq (nz_rlen max_len) in
    let len = Pvec.length is in
    assert (Pvec.is_filled is 0);
    assert (Pvec.is_filled is (len - 1));
    assert (Pvec.is_filled is (rand len));
    assert (not @@ Pvec.is_filled is len);
    assert (not @@ Pvec.is_filled is (-1));
    assert (not @@ Pvec.is_filled is (-3));
  done;
  ()

let test_affixes () =
  log "Testing Pvec.is_{prefix,infix,suffix}";
  let v0 = Pvec.of_list [1;2;3;4] in
  let v1 = Pvec.of_list [1;1;2;3] in
  let v2 = Pvec.of_list [0;1;2;3] in
  let p0 = Pvec.of_list [1] in
  let p1 = Pvec.of_list [1;2] in
  let p2 = Pvec.of_list [1;2;3] in
  let p3 = Pvec.of_list [3;2;1] in
  let s0 = Pvec.of_list [3] in
  let s1 = Pvec.of_list [2;3] in
  let s2 = Pvec.of_list [1;2;3] in
  assert Pvec.(is_prefix ~eq ~affix:empty v0);
  assert Pvec.(is_prefix ~eq ~affix:p0 v0);
  assert Pvec.(is_prefix ~eq ~affix:p1 v0);
  assert Pvec.(is_prefix ~eq ~affix:p2 v0);
  assert Pvec.(is_prefix ~eq ~affix:v0 v0);
  assert Pvec.(not @@ is_prefix ~eq ~affix:v0 p0);
  assert Pvec.(not @@ is_prefix ~eq ~affix:v0 p1);
  assert Pvec.(not @@ is_prefix ~eq ~affix:v0 p2);
  assert Pvec.(not @@ is_prefix ~eq ~affix:v0 p3);
  assert Pvec.(is_infix ~eq ~affix:empty v1);
  assert Pvec.(is_infix ~eq ~affix:v1 v1);
  assert Pvec.(is_infix ~eq ~affix:p0 v1);
  assert Pvec.(is_infix ~eq ~affix:p1 v1);
  assert Pvec.(is_infix ~eq ~affix:p2 v1);
  assert Pvec.(is_infix ~eq ~affix:p0 v2);
  assert Pvec.(is_infix ~eq ~affix:p1 v2);
  assert Pvec.(is_infix ~eq ~affix:p2 v2);
  assert Pvec.(not @@ is_infix ~eq ~affix:v2 p0);
  assert Pvec.(not @@ is_infix ~eq ~affix:v2 p1);
  assert Pvec.(not @@ is_infix ~eq ~affix:v2 p2);
  assert Pvec.(is_suffix ~eq ~affix:s0 v1);
  assert Pvec.(is_suffix ~eq ~affix:s0 v2);
  assert Pvec.(is_suffix ~eq ~affix:s1 v1);
  assert Pvec.(is_suffix ~eq ~affix:s1 v2);
  assert Pvec.(is_suffix ~eq ~affix:s2 v1);
  assert Pvec.(is_suffix ~eq ~affix:s2 v2);
  assert Pvec.(not @@ is_suffix ~eq ~affix:v1 s2);
  assert Pvec.(not @@ is_suffix ~eq ~affix:v2 s2);
  for i = 1 to repeat_count do
    let len = rlen max_len in
    let r0, r1 = rrange ~vlen:len in
    let is = iseq len in
    let pfx = Pvec.ints ~start:0 r0 in
    let ifx = Pvec.ints ~start:r0 r1 in
    let sfx = Pvec.ints ~start:r1 (len - 1) in
    assert Pvec.(is_prefix ~eq ~affix:empty is);
    assert Pvec.(is_prefix ~eq ~affix:pfx is);
    assert Pvec.(is_empty ifx || r0 = 0 || not @@ is_prefix ~eq ~affix:ifx is);
    assert Pvec.(is_empty sfx || r1 = 0 || not @@ is_prefix ~eq ~affix:sfx is);
    assert Pvec.(is_infix ~eq ~affix:empty is);
    assert Pvec.(is_infix ~eq ~affix:pfx is);
    assert Pvec.(is_infix ~eq ~affix:ifx is);
    assert Pvec.(is_infix ~eq ~affix:sfx is);
    assert Pvec.(is_suffix ~eq ~affix:empty is);
    assert Pvec.(is_suffix ~eq ~affix:sfx is);
    assert Pvec.(is_empty ifx || r1 = len - 1 ||
                 not @@ is_suffix ~eq ~affix:ifx is);
    assert Pvec.(is_empty pfx || r0 = len - 1 ||
                 not @@ is_suffix ~eq ~affix:pfx is);
  done;
  ()

let test_bool_traversals () =
  log "Testing Pvec.{for_all[i],exists[i]}";
  let always _ = true in
  let alwaysi _ _ = true in
  let never _ = false in
  let neveri _ _ = false in
  assert Pvec.(not @@ for_all always empty);
  assert Pvec.(not @@ for_alli alwaysi empty);
  assert Pvec.(not @@ for_all never empty);
  assert Pvec.(not @@ for_alli neveri empty);
  assert Pvec.(not @@ exists always empty);
  assert Pvec.(not @@ existsi alwaysi empty);
  assert Pvec.(not @@ exists never empty);
  assert Pvec.(not @@ existsi neveri empty);
  for i = 1 to repeat_count do
    let len = rlen max_len in
    let ridx = rand len in
    let is_ridx i = i = ridx in
    let is_first i = i = 0 in
    let is_last i = i = len - 1 in
    let is_self i = i = i in
    let is = iseq len in
    assert (Pvec.exists always is);
    assert (Pvec.for_all always is);
    assert (not @@ Pvec.exists never is);
    assert (not @@ Pvec.for_all never is);
    assert (Pvec.exists is_ridx is);
    assert (not @@ Pvec.for_all is_ridx is);
    assert (Pvec.exists is_first is);
    assert (not @@ Pvec.for_all is_first is);
    assert (Pvec.exists is_last is);
    assert (not @@ Pvec.for_all is_last is);
    assert (Pvec.exists is_self is);
    assert (Pvec.for_all is_self is);
  done;
  ()

let test_getting () =
  log "Testing Pvec.{get,get_first,get_last,el,first_el,last_el}";
  assert_invalid_arg (Pvec.get Pvec.empty) (-1);
  assert_invalid_arg (Pvec.get Pvec.empty) 0;
  assert_invalid_arg (Pvec.get Pvec.empty) 1;
  assert_invalid_arg (Pvec.get_first) Pvec.empty;
  assert_invalid_arg (Pvec.get_last) Pvec.empty;
  assert Pvec.(el empty (-1) = None);
  assert Pvec.(el empty 0 = None);
  assert Pvec.(el empty 1 = None);
  assert Pvec.(first_el empty = None);
  assert Pvec.(last_el empty = None);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let ridx = rand len in
    assert_invalid_arg (Pvec.get is) (-1);
    for i = 0 to len - 1 do
     assert Pvec.(get is i = i);
    done;
    assert_invalid_arg (Pvec.get is) len;
    assert Pvec.(get_first is = 0);
    assert Pvec.(get_last is = len - 1);
    assert Pvec.(el is (-1) = None);
    assert Pvec.(el is ridx = Some ridx);
    assert Pvec.(el is len = None);
    assert Pvec.(first_el is = Some 0);
    assert Pvec.(last_el is = Some (len -1));
  done;
  ()

let test_range () =
  log "Testing Pvec.range";
  assert Pvec.(is_empty @@ range ~first:0 ~last:0 empty);
  assert Pvec.(is_empty @@ range ~first:0 ~last:1 empty);
  assert Pvec.(is_empty @@ range ~first:1 ~last:0 empty);
  assert Pvec.(is_empty @@ range ~first:(-1) ~last:(-2) empty);
  assert Pvec.(is_empty @@ range ~first:34 ~last:36 empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let first, last = rrange len in
    let ir = Pvec.range ~first ~last is in
    assert Pvec.(is_empty @@ range ~first:(-32) ~last:(-1) is);
    assert Pvec.(is_empty @@ range ~first:len ~last:(len + 3) is);
    assert Pvec.(equal ~eq is (range ~first:(-32) ~last:(len + 1) is));
    assert Pvec.(equal ~eq is (range ~first:0 ~last:(len - 1) is));
    assert Pvec.(equal ~eq
                   (range ~first:0 ~last is) (range ~first:(-1) ~last is));
    assert Pvec.(equal ~eq
              (range ~first:last ~last:(len - 1) is)
              (range ~first:last ~last:(len + 32) is));
    for i = 0 to Pvec.length ir - 1 do assert Pvec.(get ir i = first + i) done;
    assert Pvec.(is_infix ~eq ~affix:ir is);
  done;
  ()

let test_append () =
  log "Testing Pvec.append";
  assert Pvec.(is_empty (empty ++ empty));
  for i = 1 to repeat_count do
    let is0 = iseq (rlen max_len) in
    let is1 = iseq (rlen max_len) in
    let is = Pvec.(is0 ++ is1) in
    assert Pvec.(equal ~eq is0 is0);
    assert Pvec.(equal ~eq is0 (is0 ++ empty));
    assert Pvec.(equal ~eq is0 (empty ++ is0));
    assert Pvec.(equal ~eq
                   (range ~first:0 ~last:(length is0 - 1) is) is0);
    assert Pvec.(equal ~eq
                   (range ~first:(length is0) ~last:(length is - 1) is) is1);
  done;
  ()

let test_push () =
  log "Testing Pvec.add_{first,last}";
  assert Pvec.(get (add_first (-1) empty) 0 = -1);
  assert Pvec.(get (add_last empty (-1)) 0 = -1);
  for i = 1 to repeat_count do
    let len = rlen max_len in
    let is = iseq len in
    assert Pvec.(get (add_first (-1) is) 0 = -1);
    assert Pvec.(get (add_last is (-1)) len = -1);
  done;
  ()

let test_concat () =
  log "Testing Pvec.{concat,concat_list}";
  let sep = iseq (rlen max_len) in
  assert Pvec.(is_empty @@ concat ~sep empty);
  assert Pvec.(is_empty @@ concat empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let iss = Pvec.init ~len:len (fun i -> iseq 64) in
    let mcat = ref (Pvec.get iss 0) in
    let mcat_sep = ref (Pvec.get iss 0) in
    for i = 1 to Pvec.length iss - 1 do
      mcat := Pvec.(!mcat ++ get iss i);
      mcat_sep := Pvec.(!mcat_sep ++ sep ++ get iss i);
    done;
    assert Pvec.(equal ~eq (concat iss) !mcat);
    assert Pvec.(equal ~eq (concat ~sep iss) !mcat_sep);
    assert Pvec.(equal ~eq (concat_list ~sep (to_list iss)) !mcat_sep)
  done;
  ()

let test_splice () =
  log "Testing Pvec.splice";
  for i = 1 to repeat_count + 3 do
    let len0 = if i = 0 || i = 2 then 0 else rlen max_len in
    let len1 = if i = 0 || i = 1 then 0 else rlen max_len in
    let first, last = if len0 = 0 then 0, 0 else rrange len0 in
    let is0 = iseq len0 in
    let is1 = iseq len1 in
    assert Pvec.(equal ~eq
                   (splice ~into:is0 ~first ~last is1)
                   (take_left first is0 ++ is1 ++ drop_left (last + 1) is0));
    assert Pvec.(equal ~eq
                   (splice ~into:is0 ~first is1)
                   (take_left first is0 ++ is1 ++ drop_left first is0));
    assert Pvec.(equal ~eq
                   (splice ~into:is0 ~first:(-1) is1) (is1 ++ is0));
    assert Pvec.(equal ~eq
                   (splice ~into:is0 ~first:len0 is1) (is0 ++ is1));
    assert Pvec.(equal ~eq
                   (splice ~into:is0 ~first:len0 ~last:(len0 + 1) is1)
                   (is0 ++ is1));
    assert Pvec.(equal ~eq
                   (splice ~into:is0 ~first:0 ~last:(len0 - 1) is1)
                   is1);
    assert Pvec.(equal ~eq
                   (splice ~into:is0 ~first:(-1) ~last:len0 is1)
                   is1);
  done;
  ()

let test_setting () =
  log "Testing Pvec.{set,set_first,set_last,fill,fill_first,fill_last}";
  let filling = Pvec.of_list [0; 0; 0; (-1)] in
  let mone = Pvec.singleton (-1) in
  assert_invalid_arg  Pvec.(set empty 0) 3;
  assert_invalid_arg  Pvec.(set_first empty) 3;
  assert_invalid_arg  Pvec.(set_last empty) 3;
  assert Pvec.(equal ~eq (fill empty ~pad:0 3 (-1)) filling);
  assert Pvec.(equal ~eq (fill_first empty (-1)) mone);
  assert Pvec.(equal ~eq (fill_last empty (-1)) mone);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let ridx = rand len in
    assert Pvec.(equal ~eq
                   (set is ridx (-1))
                   (splice ~into:is ~first:ridx ~last:ridx mone));
    assert Pvec.(equal ~eq
                   (set_first is (-1))
                   (splice ~into:is ~first:0 ~last:0 mone));
    assert Pvec.(equal ~eq
                   (set_last is (-1))
                   (splice ~into:is ~first:(len - 1) ~last:(len - 1) mone));
    assert Pvec.(equal ~eq
                   (fill ~pad:0 is (len + 3) (-1))
                   (is ++ filling));
    assert Pvec.(equal ~eq
                   (fill ~pad:0 is 0 (-1))
                   (splice ~into:is ~first:0 ~last:0 mone));
    assert Pvec.(equal ~eq
                   (fill_first is (-1))
                   (splice ~into:is ~first:0 ~last:0 mone));
    assert Pvec.(equal ~eq
                   (fill_last is (-1))
                   (splice ~into:is ~first:(len - 1) ~last:(len - 1) mone));
  done;
  ()

let test_removing () =
  log "Testing Pvec.{rem_range,rem,rem_first,rem_last}";
  assert Pvec.(is_empty @@ rem_range ~first:(-1) ~last:0 empty);
  assert Pvec.(is_empty @@ rem_range ~first:1 ~last:10 empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let ridx = rand len in
    let first, last = rrange len in
    assert Pvec.(equal ~eq
                   (rem_range ~first ~last is)
                   (take_left first is ++ drop_left (last + 1) is));
    assert Pvec.(equal ~eq (rem_range ~first ~last:(first - 1) is) is);
    assert Pvec.(equal ~eq
                   (rem_range ~first:(-1) ~last is)
                   (drop_left (last + 1) is));
    assert Pvec.(equal ~eq
                   (rem_range ~first ~last:(len + 2) is)
                   (take_left first is));
    assert Pvec.(equal ~eq
                   (rem is ridx)
                   (splice ~into:is ~first:ridx ~last:ridx empty));
    assert Pvec.(equal ~eq (rem_first is) (drop_left 1 is));
    assert Pvec.(equal ~eq (rem_last is) (drop_right 1 is));
  done;
  ()

let test_traversing () =
  log "Testing Pvec.{fold,foldi,iter,iteri}_{left,right}";
  let never _ = assert false in
  let never2 _ _ = assert false in
  Pvec.(foldi_left never () empty);
  Pvec.(foldi_right never empty ());
  Pvec.(fold_left never () empty);
  Pvec.(fold_right never empty ());
  Pvec.(iteri_left never2 empty);
  Pvec.(iteri_right never2 empty);
  Pvec.(iter_left never empty);
  Pvec.(iter_right never empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let is_sum = iseq_sum is in
    let fl = let i = ref 0 in fun sum v -> assert (!i = v); incr i; sum + v in
    assert (Pvec.fold_left fl 0 is = is_sum);
    let fr = let i = ref len in fun v sum -> decr i; assert (!i = v); sum + v in
    assert (Pvec.fold_right fr is 0 = is_sum);
    let fil = fun sum i v -> assert (i = v); sum + v in
    assert (Pvec.foldi_left fil 0 is = is_sum);
    let fir = fun i v sum -> assert (i = v); sum + v in
    assert (Pvec.foldi_right fir is 0 = is_sum);
    let () =
      let i = ref 0 and sum = ref 0 in
      let il = fun j k -> assert (!i = j && j = k); incr i; sum := !sum + k in
      Pvec.iteri_left il is; assert (!sum = is_sum);
    in
    let () =
      let i = ref len and sum = ref 0 in
      let ir = fun j k -> decr i; assert (!i = j && j = k); sum := !sum + k in
      Pvec.iteri_right ir is; assert (!sum = is_sum);
    in
    let () =
      let i = ref 0 and sum = ref 0 in
      let il = fun k -> assert (!i = k); incr i; sum := !sum + k in
      Pvec.iter_left il is; assert (!sum = is_sum);
    in
    let () =
      let i = ref len and sum = ref 0 in
      let ir = fun k -> decr i; assert (!i = k); sum := !sum + k in
      Pvec.iter_right ir is; assert (!sum = is_sum);
    in
    ()
  done;
  ()

let test_map () =
  log "Testing Pvec.{map[i],filter_map[i]}";
  assert Pvec.(is_empty @@ mapi (fun i _ -> i) empty);
  assert Pvec.(is_empty @@ map (fun _ -> 1) empty);
  assert Pvec.(is_empty @@ filter_mapi (fun i _ -> Some i) empty);
  assert Pvec.(is_empty @@ filter_map (fun _ -> Some 1) empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let p0 = Pvec.mapi (fun i v -> (i, v)) is  in
    let p1 = Pvec.map (fun v -> (v, v)) is  in
    let is' = Pvec.map (fun (_, v) -> v) p0 in
    assert Pvec.(equal ~eq p0 p1);
    assert Pvec.(equal ~eq is is');
    let keep_pair v = if v mod 2 = 0 then Some v else None in
    let pair0 = Pvec.filter_map keep_pair is in
    let keep_pair i _ = if i mod 2 = 0 then Some i else None in
    let pair1 = Pvec.filter_mapi keep_pair is in
    assert Pvec.(for_alli (fun i e -> i * 2 = e) pair0);
    assert Pvec.(length pair0 = (len + 2 - 1) / 2 (* ceil *));
    assert Pvec.(for_alli (fun i e -> i * 2 = e) pair1);
    assert Pvec.(length pair1 = (len + 2 - 1) / 2 (* ceil *));
  done;
  ()

let test_rev () =
  log "Testing Pvec.rev";
  assert Pvec.(is_empty @@ rev empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let is_sum = iseq_sum is in
    let isr = Pvec.rev is in
    assert (is_sum = iseq_sum isr);
    assert Pvec.(equal ~eq is (rev isr));
    for i = 0 to Pvec.length is - 1 do
      assert Pvec.(get is i = get isr (len - 1 - i));
    done
  done;
  ()

let test_indices () =
  log "Testing Pvec.indices";
  assert Pvec.(is_empty @@ indices empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    assert Pvec.(equal ~eq is (indices is));
  done;
  ()

let test_shuffle () =
  log "Testing Pvec.shuffle";
  assert Pvec.(is_empty @@ shuffle ~rand empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let isr = Pvec.shuffle ~rand is in
    assert (iseq_sum is = iseq_sum isr);
  done;
  ()

let test_unstutter () =
  log "Testing Pvec.unstutter";
  assert Pvec.(is_empty @@ unstutter ~eq empty);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    for m = 2 to 4 do
      let st = Pvec.map (fun i -> i - (i mod m)) is in
      let ust = Pvec.unstutter ~eq st in
      assert Pvec.(for_alli (fun i e -> i * m = e) ust);
      assert Pvec.(length ust = (len + m - 1) / m (* ceil *));
    done;
  done;
  ()

let test_take_drop_break () =
  log "Testing Pvec.{take,drop,break}_{left,right}";
  let test_empty f =
    assert Pvec.(is_empty @@ f (-1) empty);
    assert Pvec.(is_empty @@ f 0 empty);
    assert Pvec.(is_empty @@ f 1 empty);
  in
  test_empty Pvec.take_left;
  test_empty Pvec.take_right;
  test_empty Pvec.drop_left;
  test_empty Pvec.drop_right;
  test_empty (fun c v -> fst (Pvec.break_left c v));
  test_empty (fun c v -> snd (Pvec.break_left c v));
  test_empty (fun c v -> fst (Pvec.break_right c v));
  test_empty (fun c v -> snd (Pvec.break_right c v));
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let rcount = rand len in
    let break_left i fst snd =
      let tl = Pvec.take_left i is in
      let dl = Pvec.drop_left i is in
      let bl_l, bl_r = Pvec.break_left i is in
      assert Pvec.(equal ~eq tl fst);
      assert Pvec.(equal ~eq dl snd);
      assert Pvec.(equal ~eq tl bl_l);
      assert Pvec.(equal ~eq dl bl_r);
    in
    let break_right i fst snd =
      let dr = Pvec.drop_right i is in
      let tr = Pvec.take_right i is in
      let br_l, br_r = Pvec.break_right i is in
      assert Pvec.(equal ~eq dr fst);
      assert Pvec.(equal ~eq tr snd);
      assert Pvec.(equal ~eq dr br_l);
      assert Pvec.(equal ~eq tr br_r);
    in
    break_left (-1) Pvec.empty is;
    break_left 0 Pvec.empty is;
    break_left rcount
      (iseq rcount)
      (Pvec.map (fun i -> i + rcount) (iseq (len - rcount)));
    break_right (-1) is Pvec.empty;
    break_right 0 is Pvec.empty;
    break_right rcount
      (iseq (len - rcount))
      (Pvec.map (fun i -> i + len - rcount) (iseq rcount))
  done;
  ()

let test_chunk () =
  log "Test Pvec.chunk_{left,right}";
  let eq = Pvec.equal ~eq in
  assert_invalid_arg (Pvec.chunk_left (-1)) Pvec.empty;
  assert_invalid_arg (Pvec.chunk_left 0) Pvec.empty;
  assert Pvec.(equal ~eq (chunk_left 1 empty) (singleton empty));
  assert Pvec.(equal ~eq (chunk_left 3 (Pvec.of_list [1;2;3;4;5;6;7;8]))
                 (Pvec.of_list [Pvec.of_list [1;2;3]; Pvec.of_list [4;5;6];
                                Pvec.of_list [7;8]]));
  assert_invalid_arg (Pvec.chunk_right (-1)) Pvec.empty;
  assert_invalid_arg (Pvec.chunk_right 0) Pvec.empty;
  assert Pvec.(equal ~eq (chunk_right 1 empty) (singleton empty));
  assert Pvec.(equal ~eq (chunk_right 3 (Pvec.of_list [1;2;3;4;5;6;7;8]))
                 (Pvec.of_list [Pvec.of_list [1;2]; Pvec.of_list [3;4;5];
                                Pvec.of_list [6;7;8]]));
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    let size = rlen len in
    assert_invalid_arg (Pvec.chunk_left (-1)) is;
    assert_invalid_arg (Pvec.chunk_right 0) is;
    assert Pvec.(equal ~eq (singleton is) (chunk_left len is));
    assert Pvec.(equal ~eq (singleton is) (chunk_left (len + 1) is));
    assert Pvec.(equal ~eq (singleton is) (chunk_right len is));
    assert Pvec.(equal ~eq (singleton is) (chunk_right (len + 1) is));
    let cl = Pvec.chunk_left size is in
    let cr = Pvec.chunk_right size is in
    let small_size = len mod size in
    assert Pvec.(length (get_last cl) = small_size);
    Pvec.(iter_left (fun v -> assert (length v = size)) (rem_last cl));
    assert Pvec.(equal ~eq:( = ) is (concat cl));
    assert Pvec.(length (get_first cr) = small_size);
    Pvec.(iter_left (fun v -> assert (length v = size)) (rem_first cr));
    assert Pvec.(equal ~eq:( = ) is (concat cr));
  done;
  ()

let test_pop () =
  log "Text Pvec.pop_{first,last}";
  assert Pvec.(pop_first empty = None);
  assert Pvec.(pop_last empty = None);
  for i = 1 to repeat_count do
    let len = nz_rlen max_len in
    let is = iseq len in
    begin match Pvec.pop_first is with
    | None -> assert false
    | Some (h, tl) ->
        assert (h = Pvec.get_first is);
        assert Pvec.(equal ~eq tl (drop_left 1 is));
    end;
    begin match Pvec.pop_last is with
    | None -> assert false
    | Some (tl, h) ->
        assert (h = Pvec.get_last is);
        assert Pvec.(equal ~eq tl (drop_right 1 is));
    end;
  done;
  ()

let test () =
  let reproduce =
    strf "Run ['%s' '--rseed' '%d' '--rlen' '%d' --repeat '%d'] to reproduce"
      Sys.argv.(0) rseed max_len repeat_count
  in
  try
    Printexc.record_backtrace true;
    test_base ();
    test_conversions ();
    test_length ();
    test_eq ();
    test_compare ();
    test_predicates ();
    test_affixes ();
    test_bool_traversals ();
    test_getting ();
    test_range ();
    test_append ();
    test_push ();
(*    test_concat (); *)
    test_splice ();
    test_setting ();
    test_removing ();
    test_traversing ();
    test_map ();
    test_rev ();
    test_indices ();
    test_shuffle ();
    test_unstutter ();
    test_take_drop_break ();
    test_chunk ();
    test_pop ();
    log "[OK] %s" reproduce;
    log "[OK] All tests succeded!";
  with
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      log "%s\n%s\n[FAIL] A test failed!"
        (Printexc.to_string e) (Printexc.raw_backtrace_to_string bt);
      log "[FAIL] %s" reproduce;
      exit 1

let () = test ()

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
