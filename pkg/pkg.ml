#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "pvec" @@ fun c ->
  Ok [ Pkg.mllib "src/pvec.mllib";
       Pkg.lib "src/pvec_top_init.ml";
       Pkg.test "test/test"; ]
