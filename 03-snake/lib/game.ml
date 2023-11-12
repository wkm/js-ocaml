open! Base

type t =
  { mutable snake : Snake.t
  ; mutable apple : Apple.t
  ; mutable game_state : Game_state.t
  ; height : int
  ; width : int
  ; amount_to_grow : int
  }
[@@deriving sexp_of]

(* [in_bounds] gives [true] if the position is within bounds of the game. *)
let in_bounds t position =
  let { Position.row; col } = position in
  0 <= row && row < t.height && 0 <= col && col < t.width
;;

(* [snake_in_bounds] gives [true] if all locations of the snake are in bounds. *)
let snake_in_bounds t snake = List.for_all (Snake.locations snake) ~f:(in_bounds t)

let create ~height ~width ~initial_snake_length ~amount_to_grow =
  if initial_snake_length > width
  then failwith "unable to create initial snake"
  else (
    let snake = Snake.create ~length:initial_snake_length in
    let apple = Apple.create ~height ~width ~invalid_locations:(Snake.locations snake) in
    match apple with
    | None -> failwith "unable to create initial apple"
    | Some apple ->
      { snake; apple; height; width; amount_to_grow; game_state = Game_state.In_progress })
;;

let snake t = t.snake
let apple t = t.apple
let game_state t = t.game_state

let set_direction t direction =
  let { snake; _ } = t in
  t.snake <- Snake.set_direction snake direction;
  ()
;;

(* TODO: Implement [step].

   [step] should:
   - move the snake forward one square
   - check for collisions (end the game with "Wall collision" or "Self collision")
   - if necessary:
     -- consume apple
     -- if apple cannot be regenerated, win game; otherwise, grow the snake *)
let step t =
  let { snake; amount_to_grow; _ } = t in
  (* move the snake forward *)
  let nextSnake = Snake.step snake in
  (* validate this state *)
  match nextSnake with
  | None -> t.game_state <- Game_state.Game_over "Self collision"
  | Some snk ->
    (* update the game's snake *)
    t.snake <- snk;
    (* validate conditions *)
    if not (snake_in_bounds t snk)
    then t.game_state <- Game_state.Game_over "Wall collision"
    else (
      let { apple; _ } = t in
      if [%compare.equal: Position.t] (Apple.location apple) (Snake.head_location snk)
      then (
        (* grow snk *)
        t.snake <- Snake.grow_over_next_steps snk amount_to_grow;
        let newApple =
          Apple.create
            ~height:t.height
            ~width:t.width
            ~invalid_locations:(Snake.locations snk)
        in
        match newApple with
        (* win the game if no apple can be placed *)
        | None -> t.game_state <- Game_state.Win
        | Some a -> t.apple <- a))
;;

module For_testing = struct
  let create_apple_force_location_exn ~height ~width ~location =
    let invalid_locations =
      List.init height ~f:(fun row ->
        List.init width ~f:(fun col -> { Position.row; col }))
      |> List.concat
      |> List.filter ~f:(fun pos -> not ([%compare.equal: Position.t] location pos))
    in
    match Apple.create ~height ~width ~invalid_locations with
    | None -> failwith "[Apple.create] returned [None] when [Some _] was expected!"
    | Some apple -> apple
  ;;

  let create_apple_and_update_game_exn t ~apple_location =
    let apple =
      create_apple_force_location_exn
        ~height:t.height
        ~width:t.width
        ~location:apple_location
    in
    t.apple <- apple
  ;;

  let create_game_with_apple_exn
    ~height
    ~width
    ~initial_snake_length
    ~amount_to_grow
    ~apple_location
    =
    let t = create ~height ~width ~initial_snake_length ~amount_to_grow in
    create_apple_and_update_game_exn t ~apple_location;
    t
  ;;
end
