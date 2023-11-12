open! Base

type t =
  { (* [direction] represents the orientation of the snake's head. *)
    direction : Direction.t
  ; (* [extensions_remaining] represents how many more times we should extend the
       snake. *)
    extensions_remaining : int
  ; (* [locations] represents the current set of squares that the snake
       occupies. The first element of the list is the head of the snake. We hold
       as an invariant that [locations] is always non-empty. *)
    locations : Position.t list
  }
[@@deriving sexp_of]

let create ~length =
  { direction = Direction.Right
  ; extensions_remaining = 0
  ; locations =
      List.range 0 length
      |> List.map ~f:(fun x -> { Position.row = 0; Position.col = x })
      |> List.rev
  }
;;

let grow_over_next_steps t by_how_much = { t with extensions_remaining = by_how_much }
let locations t = t.locations
let head_location t = List.hd_exn t.locations
let set_direction t direction = { t with direction }

let step t =
  (* move head to 'direction, delete the last item, check for collisions *)
  let front = head_location t in
  let next = Direction.next_position t.direction front in
  let movedFront = List.cons next t.locations in
  let movedTail =
    if t.extensions_remaining = 0
    then (* remove the tail location *)
      List.take movedFront (List.length movedFront - 1)
    else movedFront
  in
  match List.contains_dup movedTail ~compare:Position.compare with
  | true -> None
  | false ->
    Some
      { t with
        locations = movedTail
      ; extensions_remaining = max 0 (t.extensions_remaining - 1)
      }
;;
