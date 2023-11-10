open! Base
open! Stdio
open Snake_lib

let%expect_test "Testing [Hashset]..." =
  let hs = Hashset.create () in
  Hashset.add hs 2;
  Hashset.add hs 3;
  Hashset.add hs 4;
  print_s (Hashset.sexp_of_t hs);
  [%expect {| ((ht ((2 ()) (3 ()) (4 ())))) |}]
;;
