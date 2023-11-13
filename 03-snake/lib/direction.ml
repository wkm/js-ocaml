open! Base

type t =
  | Left
  | Up
  | Right
  | Down
[@@deriving sexp_of]

(*
   [next_position] moves in the given direction, without validating bounds
   Recall that the origin of the board is in the lower left hand corner.
*)
let next_position t position =
  let { Position.row; col } = position in
  match t with
  | Left -> { Position.col = col - 1; Position.row }
  | Right -> { Position.col = col + 1; Position.row }
  | Up -> { Position.col; Position.row = row + 1 }
  | Down -> { Position.col; Position.row = row - 1 }
;;
