open Lib

(** [invalid_move_error] is the API response when the player makes an invalid 
    move *)
let invalid_move_error = 
  let open Ezjsonm in 
  dict [("error", string "Invalid move")]

(** [invalid_start_error] is the API response when the user starts a game with
    invalid configuration *)
let invalid_start_error = 
  let open Ezjsonm in 
  dict (["error", string "Invalid start state"])

(** [json_of_board board] is the JSON representation of [board] *)
let json_of_board (board : string array array) =
  Array.map 
    (fun row -> `A (Array.map (fun grid -> `String grid) row |> Array.to_list)) 
    board
  |> Array.to_list

(** [json_of_piece piece] is the JSON representation of [piece] *)
let json_of_piece (piece : int array array) =
  Array.map 
    (fun row -> `A (Array.map 
                      (fun grid -> 
                         `Float (float_of_int grid)) row 
                    |> Array.to_list)) 
    piece
  |> Array.to_list

(** [json_of_players players] is the JSON representation of [players] *)
let json_of_players (players : Player.player list) = 
  let open Ezjsonm in 
  List.map (fun p -> 
      let pieces_set = Player.pieces p |> Player.PieceSet.elements in 
      let pieces = List.map (fun p -> Player.PieceSet.to_piece p) pieces_set in
      dict [
        ("color", string (Player.color p));
        ("pieces", `A (
            List.map 
              (fun piece -> 
                 `A (Array.map 
                       (fun row -> `A (Array.map 
                                         (fun grid -> 
                                            `Float (float_of_int grid)) 
                                         row |> Array.to_list)) piece 
                     |> Array.to_list)) pieces))
      ]
    ) players

(** [string_of_powerup powerup] is the string representation of [powerup] that 
    is shown on the UI *)
let string_of_powerup powerup = 
  let open State in 
  match powerup with 
  | Multiplier mult -> string_of_int mult ^ "x"
  | Skip -> "skip"
  | Penalty penalty -> "-" ^ string_of_int penalty

(** [json_of_powerups powerups] is the JSON representation of [powerups] *)
let json_of_powerups (powerups: ((int * int) * State.power_up) list) = 
  let open Ezjsonm in 
  List.map(fun ((row, col), powerup) -> 
      dict [
        ("row", int row);
        ("col", int col);
        ("powerup", string (string_of_powerup powerup))
      ]
    ) powerups 

(** [json_of_state st] is the JSON representation of [st] *)
let json_of_state st = 
  let open Ezjsonm in 
  let current_turn = State.current_turn st in 
  let classic_mode = State.classic_mode st in 
  dict [
    ("board", `A (State.board st |> json_of_board));
    ("players", `A (State.players st |> json_of_players));
    ("current_turn", string current_turn);
    ("classic_mode", bool classic_mode);
    ("powerups", `A (State.powerups st |> json_of_powerups))
  ]

(** [make_move_response st] is the HTTP response for the API to the /makeMove
    endpoint when the player makes a valid move *)
let make_move_response st =
  let open Ezjsonm in 
  let current_turn = State.current_turn st in 
  let is_ai = State.current_player st |> Player.ai_opponent in 
  dict [
    ("board", `A (State.board st |> json_of_board));
    ("players", `A (State.players st |> json_of_players));
    ("current_turn", string current_turn);
    ("is_ai", bool is_ai)
  ]  

(** [get_row json] is the value of the [row] field in [json] *)
let get_row json : int =
  let open Ezjsonm in 
  value json 
  |> get_dict
  |> List.assoc "row"
  |> get_int

(** [get-col json] is the value of the [col] field in [json] *)
let get_col json : int =
  let open Ezjsonm in 
  value json 
  |> get_dict
  |> List.assoc "col"
  |> get_int

(** [get_transformation json] is the value of the [get_transformation] field in 
    [json] *)
let get_transformation json : string = 
  let open Ezjsonm in 
  value json
  |> get_dict
  |> List.assoc "transformation"
  |> get_string

(** [get_piece json] is the value of the [piece] field in [json] *)
let get_piece json : int array array = 
  let open Ezjsonm in 
  let board = value json 
              |> get_dict 
              |> List.assoc "piece" 
              |> get_list (fun row -> get_list (fun grid -> get_int grid) row) 
  in
  List.map (fun row -> Array.of_list row) board |> Array.of_list

(** [get_num_players json] is the value of the [num_players] field in [json] *)
let get_num_players json: int = 
  let open Ezjsonm in 
  value json 
  |> get_dict 
  |> List.assoc "num_players"
  |> get_string
  |> int_of_string

(** [get_num_ai json] is the value of the [num_ai] field in [json] *)
let get_num_ai json: int = 
  let open Ezjsonm in 
  value json 
  |> get_dict 
  |> List.assoc "num_ai"
  |> get_string
  |> int_of_string

(** [get_ai_level json] is the value of the [ai_level] field in [json] *)
let get_ai_level json: int = 
  let open Ezjsonm in 
  value json 
  |> get_dict 
  |> List.assoc "ai_level"
  |> get_string
  |> int_of_string

(** [get_classic_mode json] is the value of the [classic_mode] field in 
    [json] *)
let get_classic_mode json: bool = 
  let open Ezjsonm in 
  value json 
  |> get_dict 
  |> List.assoc "classic_mode"
  |> get_bool