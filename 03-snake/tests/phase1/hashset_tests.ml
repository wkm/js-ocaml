open! Base
open Hashset

let%expect_test "Testing [Hashset]..." =
  let t = Hashset.create () in
  t.add 2;
  t.add 3;
  t.add 4;
  Stdio.printf !"%{sexp: t}\n%!" t;
  [%expect {||}]
;;
