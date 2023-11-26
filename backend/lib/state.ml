type power_up = 
  | Multiplier of int 
  | Skip
  | Penalty of int 

(** RI: If [classic_mode] is [true] then [scores] and [powerups] are the empty 
    list  *)
type t = {
  board: string array array;
  players: Player.player list;
  mutable turn_num: int;
  mutable scores: (string * int) list;
  classic_mode: bool;
  powerups: ((int * int) * power_up) list 
}   

(** Module definition for list of locations on the board*) 
module S = struct  
  type t = (int * int)

  let compare (x1, x2) (y1, y2) = 
    if (x1 = y1 && x2 = y2) then 0
    else if (x1 > y1) then 1
    else -1
end

(** Implementation of module S *)
module TileSet = struct
  include Set.Make(S)

  let of_pair (loc: int * int) : elt = 
    loc

  let to_pair (e: elt) : (int * int) = 
    e

  let rec to_string_aux tiles = 
    match tiles with 
    | (x, y) :: t -> 
      "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ "), " ^ to_string_aux t
    | [] -> ""

  let to_string tile_set = 
    elements tile_set 
    |> List.map to_pair 
    |> to_string_aux 

  let rec diff_aux s1 s2 = 
    match s1 with 
    | h :: t -> if mem h s2 then diff_aux t s2 else h :: diff_aux t s2
    | [] -> []

  let diff s1 s2 = 
    let result = diff_aux (elements s1) s2 in 
    of_list result

end

let player_colors_4 =
  [| "purple"; "red"; "blue"; "green" |]

let player_colors_2 = 
  [|"purple"; "red"|]

let rec init_players_aux 
    num_players 
    player_colors
    num_ai 
    ai_level = 
  if num_players > 0 then 
    let player_color = 
      player_colors.(num_players mod Array.length player_colors) in 
    (Player.init_player (num_players <= num_ai) player_color ai_level) :: 
    init_players_aux (num_players - 1) player_colors num_ai ai_level
  else []

let init_players num_players num_ai (ai_level: int) = 
  if num_players = 2 
  then init_players_aux num_players player_colors_2 num_ai ai_level
  else if num_players = 4 
  then init_players_aux num_players player_colors_4 num_ai ai_level
  else []

let init_state num_players num_ai (ai_level: int) =
  let board_size = if num_players <= 2 then 14 else 20 in 
  {
    board = Array.make_matrix board_size board_size "white";
    players = init_players num_players num_ai ai_level;
    turn_num = 0;
    scores = [];
    classic_mode = true;
    powerups = []
  }

let rec init_scores num_players player_colors = 
  if num_players > 0 
  then (player_colors.(num_players - 1), 0) :: 
       init_scores (num_players - 1) player_colors
  else []

let rec new_loc board_size powerups =    
  let row = Random.int board_size in
  let column = Random.int board_size in
  if not (List.mem_assoc (row, column) powerups)
  && row >= 5 && row <= (board_size - 5) && 
  column >= 5 && column <= (board_size - 5)
  then (row, column)
  else new_loc board_size powerups 

let get_multiplier_level () = 
  let seed = (Random.int 100) + 1 in 
  if seed <= 60 then 2 
  else if seed <= 90 then 3 
  else 4

let get_penalty_level () = 
  let seed = (Random.int 100) + 1 in 
  if seed <= 25 then 1 
  else if seed <= 50 then 2 
  else if seed <= 70 then 3 
  else if seed <= 85 then 4 
  else 5 

let gen_powerup () = 
  let seed = (Random.int 100) + 1 in 
  if seed <= 50 then 
    Multiplier (get_multiplier_level ())
  else if seed <= 80 then
    Penalty (get_penalty_level ())  
  else Skip 

let rec init_powerups_aux total_powerups board_size acc =  
  if total_powerups > 0 
  then begin
    let loc = new_loc board_size acc in 
    let powerup = gen_powerup () in 
    init_powerups_aux (total_powerups - 1) board_size ((loc, powerup) :: acc)
  end
  else acc

let init_powerups num_players =
  let board_size = if num_players = 2 then 14 else 20 in 
  let total_powerups = if num_players = 2 
    then 3
    else 6
  in
  init_powerups_aux total_powerups board_size []


let init_state_powerups num_players num_ai ai_level = 
  let board_size = if num_players = 2 then 14 else 20 in 
  let colors = if num_players = 2 then player_colors_2 else player_colors_4 in 
  let powerups = init_powerups num_players in 
  let board = Array.make_matrix board_size board_size "white" in 
  let players = init_players num_players num_ai ai_level in 
  let scores = init_scores num_players colors in 
  {
    board = board;
    players = players;
    turn_num = 0;
    scores = scores;
    classic_mode = false;
    powerups = powerups;
  }

let empty = {
  board = [|[||]|];
  players = [];
  turn_num = 0;
  scores = [];
  classic_mode = false;
  powerups = []
}

let create_state board num_players turn_num =
  {(init_state num_players 0 0) with board = board; turn_num = turn_num}

let create_state_2 board players_lst turn_num =
  {board = board; turn_num = turn_num; players = players_lst; 
   scores = []; classic_mode = false; powerups = []}

let turn_num st =
  st.turn_num

let board st = 
  st.board

let players st = 
  st.players

let classic_mode st = 
  st.classic_mode 

let powerups st = 
  st.powerups

let scores st = 
  st.scores 

let current_turn st =
  st.turn_num mod List.length st.players 
  |> List.nth st.players 
  |> Player.color

let opponents st : string list = 
  let color = current_turn st in
  let color_list = player_colors_4 |> Array.to_list in
  let opponents = List.filter (fun x -> x <> color) color_list in
  opponents

(** Takes in the current [board], player [color], selected location for the 
    piece [loc], and the selected [piece] to place, and modifies the board *)
let update_board (st) (color: string) 
    (loc: int * int) (piece : int array array) = 
  let piece_matrix = piece in 
  let length = Array.length piece_matrix in
  for i = 0 to length - 1 do
    for j = 0 to length - 1 do 
      if piece.(i).(j) = 1 
      then st.board.((fst loc) + i).((snd loc) + j) <- color 
      else (); 
    done
  done

(**2. check that the piece is not out of bounds*)
let is_in_bounds st loc piece =
  let is_valid = ref true in
  let length = Array.length piece in
  let board_length = Array.length st.board in
  for i = 0 to (length-1) do
    for j = 0 to (length - 1) do
      if piece.(i).(j) = 1 
      then begin
        is_valid := !is_valid && (fst loc) + i > -1  && (snd loc) + j > -1 &&
                    (fst loc) + i < board_length && (snd loc) + j < board_length 
      end
      else is_valid := !is_valid && true
    done
  done;
  !is_valid

(** [board] is the current state of the board, [color] is the color piece of the 
    player, [loc] is the specified location, [piece] is the selected piece. 
    [adjacent_edges board color loc piece] returns a Set of tiles that share a 
    side with the [piece] at [loc]. *)
let adjacent_edges st loc piece : TileSet.t = 
  let tile_set = ref TileSet.empty in 
  let length = Array.length piece in
  let board_length = Array.length st.board in
  for x = 0 to length - 1 do 
    for y = 0 to length - 1 do
      if piece.(x).(y) = 1
      then begin
        if (fst loc) + x - 1 > -1 && (fst loc) + x - 1 < board_length
        then tile_set := TileSet.add ((fst loc) + x - 1,(snd loc) + y) !tile_set
        else (); (*Review syntax*)
        if (fst loc) + x + 1 > -1 && (fst loc) + x + 1 < board_length
        then tile_set := TileSet.add ((fst loc) + x + 1,(snd loc) + y) !tile_set
        else ();
        if (snd loc) + y - 1 > -1 && (snd loc) + y - 1 < board_length
        then tile_set := TileSet.add ((fst loc) + x,(snd loc) + y - 1) !tile_set
        else ();
        if (snd loc) + y + 1 > -1 && (snd loc) + y + 1 < board_length
        then tile_set := TileSet.add ((fst loc) + x,(snd loc) + y + 1) !tile_set
        else ();
      end
    done
  done;
  !tile_set 

let adjacent_corners_helper loc x y board_length corner_set= 
  if (fst loc) + x - 1 > -1 && (fst loc) + x - 1 < board_length &&
     (snd loc) + y - 1 > -1 && (snd loc) + y - 1 < board_length
  then corner_set := 
      TileSet.add ((fst loc) + x - 1, (snd loc) + y - 1) !corner_set
  else ();
  if (fst loc) + x - 1 > -1 && (fst loc) + x - 1 < board_length &&
     (snd loc) + y + 1 > -1 && (snd loc) + y + 1 < board_length
  then corner_set := 
      TileSet.add ((fst loc) + x - 1, (snd loc) + y + 1) !corner_set
  else ();
  if (fst loc) + x + 1 > -1 && (fst loc) + x + 1 < board_length &&
     (snd loc) + y - 1 > -1 && (snd loc) + y - 1 < board_length
  then corner_set := 
      TileSet.add ((fst loc) + x + 1, (snd loc) + y - 1) !corner_set
  else ();
  if (fst loc) + x + 1 > -1 && (fst loc) + x + 1 < board_length &&
     (snd loc) + y + 1 > -1 && (snd loc) + y + 1 < board_length
  then corner_set := 
      TileSet.add ((fst loc) + x + 1, (snd loc) + y + 1) !corner_set
  else ();
  !corner_set

(** [board] is the current state of the board, [color] is the color piece of the 
    player, [loc] is the specified location, [piece] is the selected piece. 
    [adjacent_corners board color loc piece] returns a Set of tiles that share a 
    corner of the [piece] at [loc]. *)
let adjacent_corners st loc piece : TileSet.t =
  let corner_set = ref TileSet.empty in
  let board_length = Array.length st.board in 
  let length = Array.length piece in 
  for x = 0 to length - 1 do
    for y = 0 to length - 1 do
      if piece.(x).(y) = 1
      then begin
        corner_set := adjacent_corners_helper loc x y board_length corner_set
      end
    done
  done;
  !corner_set

let is_occupied board loc piece = 
  let occupied = ref false in 
  let length = Array.length piece in
  for x = 0 to length - 1 do
    for y = 0 to length - 1 do
      if piece.(x).(y) = 1
      then begin
        if board.((fst loc) + x).((snd loc) + y) <> "white" 
        then occupied := true
        else ();
      end
      else ();
    done
  done;
  !occupied

let piece_on_start loc piece color_loc=
  let is_valid = ref false in
  let length = Array.length piece in
  for i = 0 to (length - 1) do
    for j = 0 to (length - 1) do
      if piece.(i).(j) = 1 
      then begin
        if ((fst loc) + i,(snd loc) + j) = color_loc
        then is_valid := true
      end
    done
  done;
  !is_valid

let is_valid_start st color loc piece = 
  let max_board_index = Array.length st.board in
  if (color = "purple") then
    is_in_bounds st loc piece && piece_on_start loc piece (0,0)
  else if (color = "red") then 
    is_in_bounds st loc piece  && 
    piece_on_start loc piece (max_board_index - 1, max_board_index - 1) 
  else if (color = "green") then 
    is_in_bounds st loc piece  && 
    piece_on_start loc piece (max_board_index - 1, 0) 
  else 
    is_in_bounds st loc piece && 
    piece_on_start loc piece (0, max_board_index - 1) 

(** [board] is the current state of the board, [color] is the color piece of the 
    player, [loc] is the specified location, [piece] is the selected piece. 
    [is_valid_move board color loc piece] returns a bool that indicates no other 
    piece of the same color shares an edge and there is a shared corner with 
    another piece of the same color. Also checks if the selected location for 
    the piece is unoccupied. *)
let is_valid_move st color loc (piece : int array array) =
  if (st.turn_num < List.length st.players) then begin
    is_valid_start st color loc piece
  end
  else begin 
    if not (is_in_bounds st loc piece) then false
    else begin 
      let piece_matrix = piece in 
      let adj_tiles = adjacent_edges st loc (piece_matrix) in 
      if TileSet.exists (fun loc -> st.board.(fst loc).(snd loc) = color) 
          adj_tiles 
      then false
      else begin
        let corner_tiles = adjacent_corners st loc piece_matrix in
        if TileSet.exists (fun loc -> st.board.(fst loc).(snd loc) = color) 
            corner_tiles
        then begin 
          let piece_matrix = piece in 
          not (is_occupied st.board loc piece_matrix) 
        end
        else false
      end
    end
  end

(** Skips the current player by incrementing the turn number *)
let next_player st = 
  st.turn_num <- st.turn_num + 1

let current_player st = 
  st.turn_num mod List.length st.players 
  |> List.nth st.players 

let rec eval_powerups st powerups (m, p) =  
  match powerups with 
  | powerup :: t -> begin
      match powerup with 
      | Multiplier mult -> eval_powerups st t (m * mult, p)
      | Skip -> begin
          next_player st; 
          eval_powerups st t (m, p)
        end
      | Penalty pen -> eval_powerups st t (m, p + pen)
    end
  | [] -> (m ,p)

let rec find_powerups st tiles = 
  match tiles with 
  | h :: t -> begin
      match List.assoc_opt h st.powerups with 
      | Some powerup -> powerup :: find_powerups st t 
      | None -> find_powerups st t 
    end
  | [] -> []

let rec update_score_aux scores color num_tiles multiplier penalty = 
  match scores with 
  | (c, s) :: t -> begin
      if c = color 
      then begin
        (c, s + (num_tiles * multiplier) - penalty) :: t
      end
      else (c, s) :: update_score_aux t color num_tiles multiplier penalty 
    end
  | [] -> []

let update_score st tile_set = 
  let tiles = TileSet.elements tile_set 
              |> List.map (TileSet.to_pair) 
  in 
  let num_tiles = List.length tiles in 
  let color = current_turn st in 
  let powerups = find_powerups st tiles in 
  let (multiplier, penalty) = eval_powerups st powerups (1, 0) in
  let updated_scores = 
    update_score_aux st.scores color num_tiles multiplier penalty  
  in   
  st.scores <- updated_scores

let update_board_powerups st color (x, y) piece = 
  let length = Array.length piece in 
  let piece_tiles = ref TileSet.empty in 
  for i = 0 to length - 1 do 
    for j = 0 to length - 1 do
      if piece.(i).(j) = 1
      then begin 
        st.board.(x + i).(y + j) <- color;
        piece_tiles := 
          TileSet.add (TileSet.of_pair (x + i, y + j)) !piece_tiles;
      end
    done 
  done;
  update_score st !piece_tiles

let move_powerup st loc piece = 
  let color = current_turn st in 
  if is_valid_move st color loc piece 
  then begin 
    let current_player = current_player st in 
    update_board_powerups st color loc piece;
    Player.remove_piece piece current_player;
    next_player st;
    true
  end
  else false

(** [move board color loc piece] modifies the board if the [piece] at [loc] with 
    [color] can be placed under Blokus rules. *)
let move st loc (piece : int array array) = 
  if st.classic_mode then 
    let color = current_turn st in 
    if is_valid_move st color loc piece
    then (
      update_board st color loc piece; 
      Player.remove_piece piece (current_player st);
      next_player st;
      true
    )
    else false
  else move_powerup st loc piece 

let rec valid_orientations st color x y = function
  | [] -> false 
  | h :: t -> begin
      if is_valid_move st color (x, y) h then true
      else valid_orientations st color x y t
    end

(** [player_can_move st] returns a bool that indicates if the current player can
    make a single move on the [board]. *)
let player_can_move st = 
  let can_move = ref false in 
  let current_player = List.nth st.players 
      (st.turn_num mod (List.length st.players)) in 
  let color = Player.color current_player in 
  let pieces = Player.pieces current_player 
               |> Player.PieceSet.elements 
               |> List.map Player.PieceSet.to_piece in 
  let board_length = Array.length st.board in 
  let pieces_orientations = 
    List.map (fun p -> Piece.all_orientations p) pieces in 
  for x = 0 to board_length - 1 do
    for y = 0 to board_length - 1 do
      can_move := 
        if !can_move then true 
        else List.exists 
            (fun p -> valid_orientations st color x y p) 
            pieces_orientations
    done
  done;
  !can_move

(** [calculate_score t color] calculates the score of a given player [color] 
    based on the number of tiles the player has. *)
let calculate_score st color = 
  let score = ref 0 in 
  let length = Array.length st.board in
  for i = 0 to length - 1 do
    for j = 0 to length - 1 do
      if st.board.(i).(j) = color
      then score := !score + 1 
    done
  done;
  !score

(** [winner board players] is the player that wins given the [board] *)
let winner st : string = 
  let players = List.map (fun p -> let color = Player.color p in 
                           (color, calculate_score st color)) st.players in 
  let sorted_player_scores = List.sort (fun (_, s1) (_, s2) -> s2 - s1) 
      players in 
  List.hd sorted_player_scores 
  |> fst

let winner_powerups st : string = 
  let sorted_scores = List.sort (fun (_, s1) (_, s2) -> s2 - s1) st.scores in 
  List.hd sorted_scores 
  |> fst 

(* clockwise counterclockwise vertical horizontal *)

let rotate_cw_piece (st : t) (piece : int array array) =
  let new_piece = Piece.rotate_cw piece 
                  |> Piece.align_piece
  in 
  let current_player = current_player st in 
  Player.replace_piece piece new_piece current_player

let rotate_ccw_piece (st : t) (piece : int array array) =
  let new_piece = Piece.rotate_ccw piece 
                  |> Piece.align_piece
  in
  let current_player = current_player st in 
  Player.replace_piece piece new_piece current_player

let reflect_x_piece (st : t) (piece : int array array) =
  let new_piece = Piece.reflect_x piece 
                  |> Piece.align_piece
  in 
  let current_player = current_player st in 
  Player.replace_piece piece new_piece current_player

let reflect_y_piece (st : t) (piece : int array array) =
  let new_piece = Piece.reflect_y piece 
                  |> Piece.align_piece
  in 
  let current_player = current_player st in 
  Player.replace_piece piece new_piece current_player

let rotate_piece (st : t) (piece : int array array) (transform : string) = 
  match transform with
  | "clockwise" -> rotate_cw_piece st piece
  | "counterclockwise" -> rotate_ccw_piece st piece
  | "vertical" -> reflect_x_piece st piece
  | "horizontal" -> reflect_y_piece st piece
  | _ -> ()