open Opium.Std
open Lib
open Api

let st = ref State.empty

let start_game =
  post "/start" (fun req -> 
      let open Ezjsonm in 
      Handlers.start_game_handler req st
    ) 

let start_game_options = 
  App.options "/start" (fun req ->
      Handlers.start_game_options_handler req
    )

let rotate_piece =
  post "/rotatePiece" (fun req ->
      Handlers.rotate_piece_handler req st
    )

let rotate_piece_options =
  App.options "/rotatePiece" (fun req -> 
      Handlers.rotate_piece_options_handler req 
    ) 

let make_move = 
  post "/makeMove" (fun req -> 
      Handlers.make_move_handler req st 
    )

let make_move_options = 
  App.options "/makeMove" (fun req -> 
      Handlers.make_move_options_handler req
    ) 

let make_ai_move = 
  get "/makeAIMove" (fun req ->
      let open Ezjsonm in 
      Handlers.make_ai_move_handler req st
    )

let skip_turn = 
  get "/skipTurn" (fun req ->
      Handlers.skip_turn_handler req st
    )

let skip_turn_options = 
  App.options "/skipTurn" (fun req ->
      Handlers.skip_turn_options_handler req
    )

let end_game = 
  get "/endGame" (fun req -> 
      Handlers.end_game_handler req st
    )

let end_game_options = 
  App.options "/endGame" (fun req -> 
      Handlers.end_game_options_handler req
    )

let default =
  not_found (fun _req ->
      `Json Ezjsonm.(dict [("message", string "Route not found")]) 
      |> respond'
    )

let _ =
  print_endline "Blokus API is listening on PORT 5000";
  App.empty
  |> start_game
  |> start_game_options
  |> make_move_options 
  |> make_move 
  |> make_ai_move
  |> rotate_piece_options
  |> rotate_piece 
  |> skip_turn
  |> skip_turn_options
  |> end_game
  |> end_game_options
  |> default
  |> App.middleware APIMiddleware.cors_origin_middleware
  |> App.middleware APIMiddleware.cors_method_middleware
  |> App.middleware APIMiddleware.cors_headers_middleware
  |> App.run_command