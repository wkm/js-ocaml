open! Base

type t = { location : Position.t } [@@deriving sexp_of]

let location t = t.location

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
