open Opium.Std
open Lib

(** [start_game_handler req st] handles a POST request [req] to the endpoint 
    "/start" with the current state [!st]. This endpoint is called to start a 
    Blokus game. *)
let start_game_handler (req: Request.t) (st: State.t ref) = 
  req |> App.json_of_body_exn 
  |> Lwt.map (fun _json -> 
      let num_players = JsonUtils.get_num_players _json in 
      let num_ai = JsonUtils.get_num_ai _json in 
      let ai_level = JsonUtils.get_ai_level _json in 
      let classic_mode = JsonUtils.get_classic_mode _json in 
      let total_players = num_ai + num_players in 
      if total_players = 2 || total_players = 4 then begin 
        if classic_mode then begin 
          st := State.init_state total_players num_ai ai_level;
          respond (`Json (JsonUtils.json_of_state !st))
        end
        else begin 
          st := State.init_state_powerups total_players num_ai ai_level;
          respond (`Json (JsonUtils.json_of_state !st))
        end
      end
      else respond (`Json (JsonUtils.invalid_start_error))
    )

(** [start_game_options_handler req] handles an OPTIONS request to the endpoint 
    "/start" *)
let start_game_options_handler _ = 
  let open Ezjsonm in 
  respond' (`Json (dict [("message", string "success")]))

(** [rotate_piece_handler req st] handles a POST request [req] to the endpoint 
    "/rotatePiece" with the current state [!st]. This endpoint is called when a 
    player tries to rotate a piece.  *)
let rotate_piece_handler req st = 
  req |> App.json_of_body_exn
  |> Lwt.map (fun _json ->
      let piece = JsonUtils.get_piece _json in 
      let transformation = JsonUtils.get_transformation _json in
      State.rotate_piece !st piece transformation;
      respond (`Json (JsonUtils.json_of_state !st)) 
    )

(** [rotate_piece_options_handler req] handles an OPTIONS request to the
    endpoint "/rotatePiece" *)
let rotate_piece_options_handler _ = 
  let open Ezjsonm in 
  respond' (`Json (dict [("message", string "success")]))

(** [make_move_handler req st] handles a POST request [req] to the endpoint 
    "/makeMove" with current state [!st]. This endpoint is called when the  
    player tries to make a move. *)
let make_move_handler req st = 
  req |> App.json_of_body_exn
  |> Lwt.map (fun _json ->
      let row = JsonUtils.get_row _json in 
      let col = JsonUtils.get_col _json in
      let piece = JsonUtils.get_piece _json in 
      let is_successful = State.move !st (row, col) piece in 
      if is_successful 
      then respond (`Json (JsonUtils.make_move_response !st))
      else respond (`Json (JsonUtils.invalid_move_error))
    )

(** [make_move_options_handler req] handles an OPTIONS request to the endpoint 
    "/makeMove" *)
let make_move_options_handler _ = 
  let open Ezjsonm in 
  respond' (`Json (dict [("message", string "success")]))

(** [make_ai_move_handler req st] handles a GET request [req] to the endpoint 
    "/makeAIMove" with current state [!st]. This endpoint is called when the 
    player whose turn it currently is is an AI player. *)
let make_ai_move_handler _ st = 
  let st' = Ai.move !st in 
  st := st';
  State.next_player !st;
  respond' (`Json (JsonUtils.make_move_response !st)) 

(** [skip_turn_handler req st] handles a GET request [req] to the endpoint 
    "/skipTurn" with current state [!st]. This endpoint is called when the  
    current player tries to skip their turn. *)
let skip_turn_handler _ st = 
  if State.turn_num !st < List.length (State.players !st)
  then respond' (`Json (JsonUtils.invalid_move_error))
  else begin 
    State.next_player !st;
    respond' (`Json (JsonUtils.make_move_response !st))
  end

(** [skip_turn_options_handler req] handles an OPTIONS request to the endpoint 
    "/skipTurn" *)
let skip_turn_options_handler _ = 
  let open Ezjsonm in 
  respond' (`Json (dict [("message", string "success")]))

(** [end_game_handler req st] handles a GET request to the endpoint "/endGame" 
    with current state [!st]. This endpoint is called when the user tries to 
    end the game and find the winner. *)
let end_game_handler _ st = 
  let open Ezjsonm in 
  if State.classic_mode !st 
  then let winner = State.winner !st in
    respond' (`Json (dict [("winner", string winner)]))
  else 
    let winner = State.winner_powerups !st in 
    respond' (`Json (dict [("winner", string winner)]))

(** [end_game_options_handler req] handles an OPTIONS request to the endpoint 
    "/endGame" *)
let end_game_options_handler _ = 
  let open Ezjsonm in 
  respond' (`Json (dict [("message", string "success")]))
