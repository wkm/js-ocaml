open! Base

type t = { location : Position.t } [@@deriving sexp_of]

let location t = t.location

(* TODO: Implement [create].

   Make sure to inspect the mli to understand the signature of[create]. [create]
   will take in the height and width of the board area, as well as a list of
   locations where the apple cannot be generated, and create a [t] with a random
   location on the board.

   Hint:
   - You can generate a random int up to [bound] via [Random.int bound].
   - You can pick a random element out of a list using [List.random_element_exn list].
*)
let create ~height ~width ~invalid_locations =
  if height * width <= List.length invalid_locations
  then None
  else (
    let invalid_tbl = Hash_set.of_list (module Position) invalid_locations in
    let rec loop () =
      let rrow = Random.int height in
      let rcol = Random.int width in
      let loc = { Position.col = rcol; Position.row = rrow } in
      if not (Hash_set.mem invalid_tbl loc) then Some { location = loc } else loop ()
    in
    loop ())
;;
