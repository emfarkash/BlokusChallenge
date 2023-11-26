open State
open Player

(** [shuffle_list lst] is the list [lst] with its elements randomly scrambled.*)
let shuffle_list lst =
  let rand_tags = List.map (fun piece -> (Random.bits (), piece)) lst in
  let sorted_list = List.sort compare rand_tags in 
  List.map snd sorted_list

let loc_in_bounds board_length x y = 
  x >= 0 && x < board_length && y >= 0 && y < board_length

(**[num_tiles piece] is the number of tiles that a piece takes up as in the 
   number of units that are 1 in the matrix. *)
let num_tiles piece = 
  let count = ref 0 in 
  let a = piece 
          |> Array.to_list 
          |> Array.concat in
  let length = Array.length a in 
  for i=0 to length - 1 do
    if a.(i) = 1 then incr count
    else ()
  done;
  !count

(**specs pls *)
let add_piece_board loc piece board color = 
  let length = Array.length piece in
  for i = 0 to length - 1 do
    for j = 0 to length - 1 do 
      if piece.(i).(j) = 1 
      then board.((fst loc) + i).((snd loc) + j) <- color 
      else (); 
    done
  done;
  board

let tile_set_if_helper i j board_length board tile_set = 
  (loc_in_bounds board_length (i) (j) && 
   board.(i).(j) = "white") &&
  (TileSet.find_opt (TileSet.of_pair (i, j)) !tile_set) = None

let edges_helper tile_set i j board_length board = 
  if tile_set_if_helper (i + 1) (j) board_length board tile_set
  then
    tile_set := TileSet.add (TileSet.of_pair (i + 1, j)) !tile_set
  else ();
  if tile_set_if_helper (i - 1) (j) board_length board tile_set
  then
    tile_set := TileSet.add (TileSet.of_pair (i - 1, j)) !tile_set
  else ();
  if tile_set_if_helper (i) (j - 1) board_length board tile_set
  then
    tile_set := TileSet.add (TileSet.of_pair (i, j - 1)) !tile_set
  else ();
  if tile_set_if_helper i (j + 1) board_length board tile_set
  then
    tile_set := TileSet.add (TileSet.of_pair (i, j + 1)) !tile_set
  else ();
  !tile_set

let edges board color =
  let tile_set = ref TileSet.empty in 
  let board_length = Array.length board in
  for i = 0 to board_length - 1 do
    for j = 0 to board_length - 1 do 
      if board.(i).(j) = color then
        begin
          tile_set := edges_helper tile_set i j board_length board
        end
    done
  done;
  !tile_set

let tile_corners_helper tile_set i j board_length board = 
  if tile_set_if_helper (i + 1) (j + 1) board_length board tile_set 
  then
    tile_set := TileSet.add (TileSet.of_pair (i + 1, j + 1)) !tile_set
  else ();
  if tile_set_if_helper (i - 1) (j - 1) board_length board tile_set 
  then
    tile_set := TileSet.add (TileSet.of_pair (i - 1, j - 1)) !tile_set
  else ();
  if tile_set_if_helper (i + 1) (j - 1) board_length board tile_set 
  then
    tile_set := TileSet.add (TileSet.of_pair (i + 1, j - 1)) !tile_set
  else ();
  if tile_set_if_helper (i - 1) (j + 1) board_length board tile_set 
  then 
    tile_set := TileSet.add (TileSet.of_pair (i - 1, j + 1)) !tile_set
  else ();
  !tile_set

let tile_corners board color = 
  let tile_set = ref TileSet.empty in 
  let board_length = Array.length board in
  for i = 0 to board_length - 1 do
    for j = 0 to board_length - 1 do 
      if board.(i).(j) = color then
        begin
          tile_set := tile_corners_helper tile_set i j board_length board
        end
      else ();
    done
  done;
  !tile_set

let corners board color =
  let corners = tile_corners board color in 
  let edges = edges board color in 
  TileSet.diff corners edges 

let num_tile_compare piece1 piece2 = 
  let count1 = ref 0 in let count2 = ref 0 in 
  let a = piece1 |> Array.to_list |> Array.concat in
  let b = piece2 |> Array.to_list |> Array.concat in
  let length1 = Array.length a in let length2 = Array.length b in 
  for i=0 to length1 - 1 do
    if a.(i) = 1 then incr count1
    else ()
  done;
  for i=0 to length2 - 1 do
    if b.(i) = 1 then incr count2
    else ()
  done;
  if !count1 < !count2 then 1
  else if !count1 = !count2 then 0
  else ~-1

let make_tile_set loc piece =
  let tile_set = ref TileSet.empty in 
  let piece_length = Array.length piece in
  for i=0 to piece_length - 1 do
    for j=0 to piece_length - 1 do
      if piece.(i).(j) = 1 then
        tile_set := 
          TileSet.add (TileSet.of_pair ((fst loc) + i, (snd loc) + j)) !tile_set
      else ();
    done
  done;
  !tile_set

let check_piece st color piece og_piece = 
  let board_length = Array.length (State.board st) in 
  let found_move = ref false in 
  for row = 0 to board_length - 1 do
    for col = 0 to board_length - 1 do 
      if  (not !found_move) && State.is_valid_move st color (row, col) piece
      then begin 
        let player = State.current_player st in 
        let pieces = Player.pieces player in 
        let pieces' = PieceSet.remove (PieceSet.of_piece og_piece) pieces in
        Player.update_pieces pieces' player;
        State.update_board st color (row, col) piece;
        if not (State.classic_mode st) then 
          let tile_set = make_tile_set (row, col) piece in 
          State.update_score st tile_set; 
        else ();
        found_move := true
      end
      else ()
    done;
  done;
  !found_move

let rec make_move st color pieces = 
  match pieces with
  | piece :: t -> begin
      let all_orientations = Piece.all_orientations piece in
      let move_option = 
        List.find_opt (fun p -> check_piece st color p piece) all_orientations 
      in
      match move_option with 
      | Some _ -> ()
      | None -> make_move st color t
    end
  | [] -> assert false 

let orientations_pieces_lst piece_list =
  List.map (fun p -> Piece.all_orientations p) piece_list
  |> List.flatten

let num_corners piece st loc= 
  let board = State.board st in
  let color = State.current_turn st in
  let board_copy = Array.map (Array.copy) board 
                   |> Array.copy 
  in
  let board_with_added = add_piece_board loc piece board_copy color in
  let corners_before = corners board color 
                       |> TileSet.cardinal  
  in 
  let corners_after = corners board_with_added color 
                      |> TileSet.cardinal 
  in 
  corners_after - corners_before

let rec ai_zero_aux pieces =
  match pieces with 
  | [] -> []
  | h :: _ -> if num_tiles h = 5 then ai_zero_aux (shuffle_list pieces)
    else pieces
(* 
1. get pieces --> if turn pieces greater than (num pieces - 6) then skip
2. choose random piece to put
3. if that player has made less than 3 moves, piece must be <5 so choose another
random piece if it is not <5 until true
4. place that piece
 *)
let ai_zero st player = 
  let color = Player.color player in 
  let pieces = (Player.pieces player) 
               |> PieceSet.elements 
               |> List.map (fun p -> PieceSet.to_piece p)
               |> shuffle_list
  in 
  let pieces_lst = (Player.pieces player) 
                   |> PieceSet.elements 
                   |> List.map (fun p -> PieceSet.to_piece p) in
  if (List.length (pieces_lst)) < 15
  then let coin_flip = Random.bool () in 
    if coin_flip then make_move st color pieces
    else ();
  else if (List.length (pieces_lst)) > 19 
  then let new_pieces = ai_zero_aux pieces in 
    make_move st color new_pieces
  else make_move st color pieces

let ai_one st player =
  let color = Player.color player in 
  (* let board_length = Array.length (State.board st) in  *)
  let pieces = (Player.pieces player) 
               |> PieceSet.elements 
               |> List.map (fun p -> PieceSet.to_piece p)
               |> shuffle_list
  in 
  make_move st color pieces

let ai_two st player = 
  let color = Player.color player in
  let pieces = (Player.pieces player)
               |> PieceSet.elements
               |> List.map (fun p -> PieceSet.to_piece p)
               |> shuffle_list  
               |> List.sort num_tile_compare
  in 
  make_move st color pieces

(*
1. make list of all orientations of all pieces (so 8  times normal number)
2. make a ref to max = (score, loc, piece)
3. make a for loop over locations then over x and y of board
4. check if the piece at a location is valid
5. calculate score of piece at loc and compare it to max
6. if bigger than max, store it in max. else keep loooooping
7. update the state to move to the max 
*)

(**[opponents_corners board opp_colors] is the Tile list of the tiles that are
   opponent tiles (excludes the edges from the corners!)*)
let rec opponents_corners board opp_colors = 
  match opp_colors with
  | [] -> []
  | h :: t -> let tile_list = corners board h 
                              |> TileSet.elements 
                              |> List.map TileSet.to_pair in          
    tile_list :: opponents_corners board t

let rec winner_eval opponents_colors_list st = 
  match opponents_colors_list with 
  | h :: t -> (h, State.calculate_score st h) :: winner_eval t st
  | [] -> []

let winner_sort player1 player2 = 
  match (player1, player2) with 
  | (_, score1), (_, score2) -> 
    if(score1 > score2) then 1 
    else if (score1 < score2) then -1 
    else 0

let powerup_score powerup piece = 
  match powerup with
  | Multiplier mult -> mult * (num_tiles piece)
  | Skip -> 5
  | Penalty penalty -> -(penalty)

let rec powerups powerup_lst piece = 
  match powerup_lst with
  | [] -> 0
  | h :: t -> (powerup_score h piece) +  powerups t piece

let score_helper_lvl_4 st board board_with_added = 
  let opponents_colors_list = State.opponents st in
  let corners_list = opponents_corners board opponents_colors_list in
  let num_corners = List.map (List.length) corners_list in
  let new_corners_list = opponents_corners board_with_added 
      opponents_colors_list in
  let new_num_corners = List.map (List.length) new_corners_list in
  List.map2 (-) num_corners new_num_corners 

let score_helper_lvl_5 st board board_with_added = 
  let opponents_colors_list = State.opponents st in
  let opponents_winner_eval = 
    (List.sort winner_sort (winner_eval opponents_colors_list st)) in 
  let (winner, _) = List.hd opponents_winner_eval in 
  let original = opponents_corners board [winner] 
                 |> List.hd 
                 |> List.length 
  in 
  let after = opponents_corners board_with_added [winner] 
              |> List.hd 
              |> List.length 
  in after - original

let score_helper lvl st loc piece color= 
  let tiles_score = num_tiles piece in 
  let corners_score = num_corners piece st loc in
  let board = State.board st in
  let board_copy = Array.map (Array.copy) board in
  let board_with_added = add_piece_board loc piece board_copy color in
  let pieces_loc = make_tile_set loc piece 
                   |> TileSet.elements 
                   |> List.map TileSet.to_pair 
  in 
  let powerups_score = powerups (State.find_powerups st pieces_loc) piece in
  if(lvl = 3) then 
    tiles_score + corners_score + powerups_score
  else if (lvl = 4) then 
    let score_list =  score_helper_lvl_4 st board board_with_added in
    tiles_score + corners_score + 
    (score_list |> List.fold_left (+) 0) + powerups_score
  else if (lvl = 5) then 
    let attack_winner = score_helper_lvl_5 st board board_with_added
    in 
    tiles_score + corners_score + attack_winner + powerups_score
  else 0

let loc_aux st lvl piece max =
  let board_length = Array.length (State.board st) in
  let color = State.current_turn st in
  let score_max = ref max in
  for i=0 to board_length - 1 do
    for j=0 to board_length - 1 do
      if (is_valid_move st color (i, j) piece)
      then begin 
        let score = score_helper lvl st (i, j) piece color in
        let (old_max, _, _) = !score_max in
        if score > old_max then score_max := (score, (i, j), piece);
      end
      else ();
    done
  done;
  !score_max

let rec pieces_aux st lvl max pieces = 
  match pieces with 
  | h :: t -> begin
      let (max', _, _) = (loc_aux st lvl h max) in
      let (max_origin, _, _) = max in 
      if max' > max_origin
      then pieces_aux st lvl (loc_aux st lvl h max) t
      else pieces_aux st lvl max t
    end
  | [] -> max 

let ai_score_sys st player lvl = 
  let color = Player.color player in
  let pieces = (Player.pieces player)
               |> PieceSet.elements
               |> List.map (fun p -> PieceSet.to_piece p)
               |> shuffle_list 
               |> List.sort num_tile_compare
  in
  let max = (0, (0,0), [|[|0|]|]) in 
  let pieces_orientations_lst = orientations_pieces_lst pieces in 
  let (_, loc, chosen_piece) = pieces_aux st lvl max pieces_orientations_lst in
  let original_piece = chosen_piece 
                       |> Piece.all_orientations 
                       |> Piece.get_orig_piece in
  let piece_of_array = PieceSet.of_piece original_piece in
  let pieces' = PieceSet.remove (piece_of_array) (Player.pieces player) in
  Player.update_pieces pieces' player;
  State.update_board st color loc chosen_piece;
  if not (State.classic_mode st) then 
    let tile_set = make_tile_set loc chosen_piece in 
    State.update_score st tile_set
  else ()

let ai_three st player =
  ai_score_sys st player 3 

let ai_four st player =
  ai_score_sys st player 4 

(* gang up on winner: calculate who winner is and by how much. for every 5 
   points ahead, do 1.5 multiplication to their corners we want to steal. *)
let ai_five st player = 
  ai_score_sys st player 5

let run_ai lvl st player = 
  if (lvl = 0) then ai_zero st player   
  else if (lvl = 1) then ai_one st player 
  else if (lvl = 2) then ai_two st player
  else if (lvl = 3) then ai_three st player
  else if (lvl = 4) then ai_four st player
  else if (lvl = 5) then ai_five st player
  else failwith "Frontend should not allow this"

let move st =
  if (player_can_move st) then begin
    let player = current_player st in 
    let lvl = Player.ai_level player in
    run_ai lvl st player
  end;
  st