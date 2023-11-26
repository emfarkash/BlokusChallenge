open OUnit2
open Lib
open Lib.Piece
open Lib.Player
open Lib.State
open Lib.Ai
(**
   TESTING PLAN: 

   We tested piece transformations extensively in OUnit because we represent
   pieces easily using matrices, which are easy to use as inputs and outputs
   of testing functions. We directly tested in OUnit most of the functions in
   the Player module that did not return an abstract type that was difficult to
   represent as a test case output. For some of the functions that returned type
   unit, we had to be more creative about coming up with test cases that could
   indirectly test if the function worked through examining some attribute that 
   was perhaps modified by the execution of that function. For our State module,
   we tested some of the more vital functions determining validity of moves in
   OUnit in order to ensure their accuracy in multiple situations. While such
   functions could have been tested manually via running the game and using
   trial and error, there are a lot of possibilities for the different cases of
   illegal moves that a player could make so we wanted to make sure we always
   accounted for those cases throughout the development of our codebase. We did
   not test all of the functions in the State module however; some of them are
   used many times in one game and it would be really obvious while playing the
   game if those were not working properly (e.g. updating the board after a
   player puts down a piece, whether the board is created to accommodate classic
   mode vs. non-classic mode, etc.) so we did not test those types of functions
   in OUnit. Also there are many different ways sets of pieces can appear on a
   Blokus board so of course we could not make a test case for every possible
   board; to account for this, we created various boards for 2 player and 4
   player games that include no pieces, a couple pieces, or a completely filled
   board. We also used randomized testing by creating a sample board that has
   16 pieces on it laid out in a legal way, which is more representative of what
   it might look like further into the game in order to test how different
   pieces might interact with each other during the game and making sure the
   rules regarding these cases hold accurately.

   Many of the functions in our AI module were tested manually because we
   incorporated randomness into generating moves (e.g. shuffling the order of
   the pieces) to create variety in the moves the AI makes; therefore, it would
   be impossible for us to know beforehand exactly what the AI would choose as
   a next move. Instead, we tested in OUnit the helper functions used to create
   the AIs that did not incorporate randomness to ensure that the AIs' moves
   were being calculated correctly. We had a similar case with trying to test
   the powerups in non-classic mode. Since the generation of the powerups
   incorporates randomness, we could not make OUnit tests for those functions.
   Instead, we tested the functionality of the powerups manually. Things on the
   frontend were of course tested manually by running the frontend while playing
   the game.

   Our test cases were mainly created using black box testing. We often either
   used TDD and wrote test cases before implementing functions, or had someone
   else who did not implement the function write test cases for it by just
   looking at the .mli file and the specs written there. We used a
   mix of black box and glass box testing for testing the AI module though. We
   wrote the test cases for those after developing the AIs, and as we decided to
   incorporate randomness into them, we could only test the helper functions for
   those. However, the tests for these helper functions were individually
   developed using black box testing (i.e. by only looking at the .mli specs
   for them when writing the test cases).

   We are sure that this method of testing ensures our system is correct because
   we tested all vital functions using OUnit tests where possible, being sure to
   cover all essential cases and all the rules that define our game. Where it is
   not possible to use OUnit testing due to randomness or abstract types, we did
   extensive manual testing in order to make sure everything worked.
*)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let rec string_of_int_row (row : int list) : string = 
  match row with 
  | h :: t -> string_of_int h ^ ", " ^ string_of_int_row t
  | [] -> "]\n"

let rec string_of_piece_aux (piece : int list list) : string =
  match piece with 
  | h :: t -> "[" ^ string_of_int_row h ^ string_of_piece_aux t
  | [] -> ""

let string_of_piece (piece : int array array) : string = 
  let list_piece = Array.map (fun row -> Array.to_list row) piece
                   |> Array.to_list
  in "\n" ^ string_of_piece_aux list_piece

let rotate_cw_test
    (name: string)
    (p: int array array)
    (expected_output: int array array) : test =
  name >:: (fun _ -> assert_equal expected_output ((rotate_cw p))
               ~printer:string_of_piece)

let rotate_ccw_test
    (name: string)
    (p: int array array)
    (expected_output: int array array) : test =
  name >:: (fun _ -> assert_equal expected_output ((rotate_ccw p)) 
               ~printer:string_of_piece)

let reflect_x_test
    (name: string)
    (p: int array array)
    (expected_output: int array array) : test =
  name >:: (fun _ -> assert_equal expected_output ((reflect_x p))
               ~printer:string_of_piece)

let reflect_y_test
    (name: string)
    (p: int array array)
    (expected_output: int array array) : test =
  name >:: (fun _ -> assert_equal expected_output ( (reflect_y p))
               ~printer:string_of_piece)

let align_piece_test 
    (name : string)
    (p : int array array)
    (expected_output : int array array) : test =
  name >:: (fun _ -> assert_equal expected_output (align_piece p) 
               ~printer:string_of_piece)

let piece_tests =
  [
    rotate_cw_test "rotate piece 2 cw once" (get_piece 2)
      [|[|0; 1|]; [|1; 1|]|];
    rotate_cw_test "rotate piece 4 cw once" (get_piece 4)
      [|[|1; 1|]; [|1; 1|]|];
    rotate_cw_test "rotate piece 5 cw once" (get_piece 5)
      [|[|0; 1; 0|]; [|0; 1; 1|]; [|0; 1; 0|]|];
    rotate_cw_test "rotate piece 8 cw once" (get_piece 8)
      [|[|0; 1; 0|]; [|0; 1; 1|]; [|0; 0; 1|]|];
    rotate_cw_test "rotate piece 12 cw once" (get_piece 12)
      [|[|0; 0; 1; 0|]; [|0; 0; 1; 1|]; [|0; 0; 0; 1|]; [|0; 0; 0; 1|]|];
    rotate_cw_test "rotate piece 14 cw once" (get_piece  14)
      [|[|1; 1; 1; 1; 1|]; [|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|];
        [|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|]|];
    rotate_cw_test "rotate piece 17 cw once" (get_piece 17)
      [|[|1; 1; 1|]; [|1; 0; 1|]; [|0; 0; 0|]|];
    rotate_ccw_test "rotate piece 8 ccw once" (get_piece 8)
      [|[|1; 0; 0|]; [|1; 1; 0|]; [|0; 1; 0|]|];
    rotate_ccw_test "rotate piece 14 ccw once" (get_piece 14)
      [|[|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|];
        [|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|]; [|1; 1; 1; 1; 1|]|];
    rotate_ccw_test "rotate piece 17 ccw once" (get_piece  17)
      [|[|0; 0; 0|]; [|1; 0; 1|]; [|1; 1; 1|]|];
    reflect_x_test "reflect piece 2 across x" (get_piece 2)
      [|[|0; 1|]; [|1; 1|]|];
    reflect_x_test "reflect piece 5 across x" (get_piece 5)
      [|[|0; 0; 0|]; [|1; 1; 1|]; [|0; 1; 0|]|];
    reflect_x_test "reflect piece 8 across x" (get_piece 8)
      [|[|0; 0; 0|]; [|1; 1; 0|]; [|0; 1; 1|]|];
    reflect_x_test "reflect piece 12 across x" (get_piece 12)
      [|[|0; 0; 0; 0|]; [|0; 0; 0; 0|]; [|1; 1; 0; 0|]; [|0; 1; 1; 1|]|];
    reflect_x_test "reflect piece 14 across x" (get_piece 14)
      [|[|1; 0; 0; 0; 0|]; [|1; 0; 0; 0; 0|]; [|1; 0; 0; 0; 0|];
        [|1; 0; 0; 0; 0|]; [|1; 0; 0; 0; 0|]|];
    reflect_y_test "reflect piece 2 across y" (get_piece 2)
      [|[|1; 1|]; [|1; 0|]|];
    reflect_y_test "reflect piece 5 across y" (get_piece 5)
      [|[|0; 1; 0|]; [|1; 1; 1|]; [|0; 0; 0|]|];
    reflect_y_test "reflect piece 8 across y" (get_piece 8)
      [|[|1; 1; 0|]; [|0; 1; 1|]; [|0; 0; 0|]|];
    reflect_y_test "reflect piece 12 across y" (get_piece 12)
      [|[|1; 1; 1; 0|]; [|0; 0; 1; 1|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|];
    reflect_y_test "reflect piece 14 across y" (get_piece 14)
      [|[|0; 0; 0; 0; 1|]; [|0; 0; 0; 0; 1|]; [|0; 0; 0; 0; 1|];
        [|0; 0; 0; 0; 1|]; [|0; 0; 0; 0; 1|]|];
    align_piece_test "five straight piece on bottom"
      [|[|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|];
        [|0; 0; 0; 0; 0|]; 
        [|1; 1; 1; 1; 1|]|]
      [|[|1; 1; 1; 1; 1|]; [|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|]; 
        [|0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0|]|];
    align_piece_test "piece 12 vertical flip then rotate_top" 
      (Piece.reflect_x [|[|0; 1; 1; 1|]; [|1; 1; 0; 0|]; [|0; 0; 0; 0|]; 
                         [|0; 0; 0; 0|]|])
      [|[|1; 1; 0; 0|]; [|0; 1; 1; 1|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|];
    align_piece_test "rotate_top on 1 x 1 piece" [|[|1|]|] [|[|1|]|]; 
    align_piece_test "rotate_top on 2 x 2 block" [|[|1; 1|]; [|1; 1|]|] 
      [|[|1; 1|]; [|1; 1|]|];
    align_piece_test "rotate_left on 3 x 3 piece with no rotation" 
      (get_piece 3 |> rotate_ccw) (get_piece 3 |> rotate_ccw);
    align_piece_test "rotate_left on 3 x 3 piece with one shift" 
      [|[|0; 1; 0|]; [|0; 1; 0|]; [|0; 1; 0|]|] (get_piece 3 |> rotate_ccw);
    align_piece_test "rotate_left on 3 x 3 piece with multiple shifts" 
      (get_piece 3 |> rotate_cw) (get_piece 3 |> rotate_ccw);
    align_piece_test "rotate_left on 1 x 1 piece" [|[|1|]|] [|[|1|]|];
    align_piece_test "rotate_left on 2 x 2 block" [|[|1; 1|]; [|1; 1|]|] 
      [|[|1; 1|]; [|1; 1|]|];
  ]

let remove_piece_test
    (name: string)
    (piece : int array array)
    (player: Player.player) 
    (expected_output: bool): test =
  name >:: (fun _ -> Player.remove_piece piece player; 
             assert_equal expected_output 
               (PieceSet.mem (Player.PieceSet.of_piece piece) 
                  (Player.pieces player)) ~printer:string_of_bool)

(** [org_player] is a level 1 AI player. *)
let org_player = init_player true "red" 1

let ai_opponent_test
    (name : string)
    (is_ai : bool)
    (color : string)
    (ai_level : int): test = 
  name >:: (fun _ -> let p = init_player is_ai color ai_level in 
             assert_equal is_ai (ai_opponent p) ~printer:string_of_bool)

let color_test 
    (name : string)
    (p : Player.player)
    (c : string) : test =
  name >:: (fun _ -> assert_equal c (color p) ~printer:(fun s -> s))

let pieces_test 
    (name : string)
    (player : Player.player)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pieces player |> PieceSet.cardinal) 
        ~printer:string_of_int)

let can_move_test 
    (name : string)
    (player : Player.player)
    (expected_output : bool) : test =
  name >:: (fun _ -> assert_equal expected_output (can_move_f player) 
               ~printer:string_of_bool)

let ai_level_test
    (name: string)
    (player: Player.player)
    (expected_output: int) : test =
  name >:: (fun _ -> assert_equal expected_output (ai_level player)
               ~printer: string_of_int)

let player_tests : OUnit2.test list =
  [ 
    ai_level_test "org_player is ai level 1" org_player 1;
    ai_opponent_test "test that init_player correctly creates a A.I. player"
      true "red" 0;
    ai_opponent_test "test that init_player correctly creates a real (not A.I.)
      player" false "red" ~-1;
    color_test "test that init_player correctly assigns a color to a player" 
      org_player "red";
    can_move_test "test that init_player has player created with can_move being 
      true" org_player true;
    pieces_test "test that init_player has player created with a full set of 
    pieces"
      org_player 21;
    remove_piece_test "remove piece test for first piece" 
      [|[|1|]|] (init_player true "red" 1) false;
    remove_piece_test "remove piece test for second piece" 
      [|[|1; 1|]; [|0; 1|]|] (init_player false "blue" ~-1) false;
    remove_piece_test "remove piece test for last piece" 
      [|[|0; 1; 0; 0|]; [|1; 1; 1; 1|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|] 
      (init_player false "blue" ~-1) false;
  ]

let rec string_of_row (row : string list) : string = 
  match row with 
  | h :: t -> h ^ ", " ^ string_of_row t 
  | [] -> "]\n"

let rec string_of_board_aux (board : string list list) : string =
  match board with 
  | h :: t -> "[" ^ string_of_row h ^ string_of_board_aux t  
  | [] -> ""

let string_of_board (board : string array array) : string =
  let list_board = Array.map (fun row -> Array.to_list row) board 
                   |> Array.to_list
  in "\n" ^ string_of_board_aux list_board

let update_board_test 
    (name: string)
    (board_t: State.t)
    (color: string)
    (loc: (int * int))
    (piece: int array array)
    (new_board: string array array) : test =
  name >:: (fun _ -> 
      update_board board_t color loc piece;
      assert_equal new_board (State.board board_t) ~printer:string_of_board)

let is_valid_move_test 
    (name: string)
    (st: State.t)
    (color: string)
    (loc: (int * int))
    (piece: int array array)
    (result: bool) : test =
  name >:: (fun _ -> assert_equal result (is_valid_move st color loc piece)
               ~printer:string_of_bool)

(** [init_st] is an initial state with 2 real players (no AI). *)
let init_st = ref (init_state 2 0 ~-1)

let calculate_score_test  
    (name: string)
    (board: State.t)
    (color: string)
    (result: int) : test =
  name >:: (fun _ -> assert_equal result (calculate_score board color) 
               ~printer: string_of_int)

let player_can_move_test
    (name: string)
    (state: State.t)
    (expected_output: bool) : test = 
  name >:: (fun _ -> assert_equal expected_output (player_can_move state)
               ~printer: string_of_bool)

let winner_test
    (name: string)
    (st: State.t)
    (expected_output: string) : test = 
  name >:: (fun _ -> assert_equal expected_output (winner st) 
               ~printer:(fun s -> s))

(** [small_board_with_purple_piece_2] is a small board with purple piece 2 at
    location (0,0) *)
let small_board_with_purple_piece_2 =
  let array = Array.make_matrix 14 14 "white" in
  array.(0).(0) <- "purple";
  array.(0).(1) <- "purple";
  array.(1).(1) <- "purple";
  array

(** [small_board_with_red_piece] is a small board with red piece 0 at location
    (13, 13). *)
let small_board_with_red_piece =
  let array = Array.make_matrix 14 14 "white" in
  array.(13).(13) <- "red";
  array

(** [small_board_with_purple_and_red] is like [small_board_with_purple_piece_2],
    but with an additional red piece 0 at (1, 2). *)
let small_board_with_purple_and_red =
  let array = Array.make_matrix 14 14 "white" in 
  array.(0).(0) <- "purple";
  array.(0).(1) <- "purple";
  array.(1).(1) <- "purple";
  array.(1).(2) <- "red";
  array

let winner_board_one =
  let array = Array.make_matrix 14 14 "white" in
  array.(13).(13) <- "red";
  array.(0).(0) <- "purple";
  array.(0).(1) <- "purple";
  array.(1).(1) <- "purple";
  array

let winner_board_two =
  let array = Array.make_matrix 20 20 "white" in
  array.(19).(0) <- "green";
  array.(18).(0) <- "green";
  array.(17).(0) <- "green";
  array.(16).(0) <- "green";
  array.(19).(19) <- "red";
  array.(0).(0) <- "purple";
  array.(0).(1) <- "purple";
  array.(1).(1) <- "purple";
  array.(0).(19) <- "blue";
  array.(0).(18) <- "blue";
  array 

let winner_board_two_plus =
  let array = Array.make_matrix 20 20 "white" in
  array.(19).(0) <- "green";
  array.(18).(0) <- "green";
  array.(17).(0) <- "green";
  array.(16).(0) <- "green";
  array.(19).(19) <- "red";
  array.(0).(0) <- "purple";
  array.(0).(1) <- "purple";
  array.(1).(1) <- "purple";
  array.(0).(19) <- "blue";
  array.(0).(18) <- "blue";
  array.(1).(17) <- "blue";
  array 

(** [big_blue_board] is a big board where blue had just made initial move at
    (0, 13). *)
let big_blue_board =
  let array = Array.make_matrix 14 14 "white" in
  array.(0).(13) <- "blue";
  array

let four_player_blue_board =
  let array = Array.make_matrix 20 20 "white" in
  array.(0).(13) <- "blue";
  array

(** [big_green_board] is a big board where green had made moves at (19, 0) and
    (19, 1). *)
let big_green_board =
  let array = Array.make_matrix 20 20 "white" in
  array.(19).(0) <- "green";
  array.(19).(1) <- "green";
  array

let full_board = let array = Array.make_matrix 20 20 "blue" in array

let full_small_board = let array = Array.make_matrix 14 14 "purple" in array

let two_piece_state = create_state winner_board_one 2 1
let big_four_player_state = create_state winner_board_two 4 2
let full_state = create_state full_board 4 5
let small_state = create_state small_board_with_purple_piece_2 2 1
let full_small_state = create_state full_small_board 2 5

(* The following are some sample players to use for testing *)

(** [patty1] is a non-AI player with no pieces. Her color is purple and it is
    her turn. *)
let patty1 =
  Player.create_player false "purple" ~-1 Player.PieceSet.empty true

(** [patty2] is a non-AI red player with no pieces. It is not her turn. *)
let patty2 =
  Player.create_player false "red" ~-1 Player.PieceSet.empty false

(** [patty3] is a non-AI purple player with only a piece 0. It is her turn. *)
let patty3 =
  Player.create_player false "purple" ~-1
    (Player.PieceSet.empty
     |> Player.PieceSet.add (Piece.get_piece 0 |> Player.PieceSet.of_piece))
    true

(** [patty4] is a non-AI red player with only a piece 0. It is not her turn. *)
let patty4 =
  Player.create_player false "red" ~-1
    (Player.PieceSet.empty
     |> Player.PieceSet.add (Piece.get_piece 0 |> Player.PieceSet.of_piece))
    false

(* [no_pieces_left_state] is a state with 2 players [patty1] and [patty2] who
   both do not have any more pieces left and the board is
   [small_board_with_purple_piece_2]. *)
let no_pieces_left_state =
  State.create_state_2 small_board_with_purple_piece_2 [patty1; patty2] 10

(* [patty_piece_zero_state] is a state with 2 players [patty2] (red) and
   [patty3] (purple). [patty3] has only a piece 0, and the board is
   [small_board_with_purple_piece_2]. It's [patty3]'s turn. *)
let patty_piece_zero_state =
  State.create_state_2 small_board_with_purple_piece_2 [patty2; patty3] 31

(* [patty_piece_zero_state_2] is a state with 2 players [patty1] (purple) and
   [patty4] (red). [patty4] has only a piece 0, and the board is
   [small_board_with_purple_piece_2]. It's [patty1]'s turn. *)
let patty_piece_zero_state_2 =
  State.create_state_2 small_board_with_purple_piece_2 [patty1; patty4] 6

(** [pattys_board_1_4p] is a sample board during a 4 player game. The turn
    number [turn_num] is 16.
    For a visual of this board, look in our google drive folder. *)
let pattys_board_1_4p =
  let array = Array.make_matrix 20 20 "white" in 
  array.(0).(0) <- "purple";
  array.(1).(0) <- "purple";
  array.(2).(0) <- "purple";
  array.(2).(1) <- "purple";
  array.(2).(2) <- "purple";
  array.(2).(4) <- "purple";
  array.(2).(5) <- "purple";
  array.(3).(3) <- "purple";
  array.(3).(4) <- "purple";
  array.(4).(3) <- "purple";
  array.(5).(4) <- "purple";
  array.(6).(4) <- "purple";
  array.(6).(5) <- "purple";
  array.(7).(4) <- "purple";
  array.(8).(4) <- "purple";
  array.(9).(5) <- "purple";
  array.(10).(5) <- "purple";
  array.(10).(6) <- "purple";
  array.(11).(5) <- "purple";
  array.(11).(6) <- "purple";
  array.(11).(8) <- "green";
  array.(12).(7) <- "green";
  array.(12).(8) <- "green";
  array.(12).(9) <- "green";
  array.(13).(6) <- "green";
  array.(13).(8) <- "green";
  array.(14).(6) <- "green";
  array.(15).(5) <- "green";
  array.(15).(6) <- "green";
  array.(15).(7) <- "green";
  array.(16).(3) <- "green";
  array.(16).(4) <- "green";
  array.(17).(2) <- "green";
  array.(17).(3) <- "green";
  array.(18).(1) <- "green";
  array.(18).(3) <- "green";
  array.(19).(0) <- "green";
  array.(19).(1) <- "green";
  array.(0).(18) <- "blue";
  array.(0).(19) <- "blue";
  array.(1).(13) <- "blue";
  array.(1).(14) <- "blue";
  array.(1).(15) <- "blue";
  array.(1).(16) <- "blue";
  array.(1).(17) <- "blue";
  array.(1).(19) <- "blue";
  array.(2).(12) <- "blue";
  array.(3).(11) <- "blue";
  array.(3).(12) <- "blue";
  array.(4).(11) <- "blue";
  array.(4).(12) <- "blue";
  array.(5).(9) <- "blue";
  array.(5).(10) <- "blue";
  array.(6).(8) <- "blue";
  array.(6).(9) <- "blue";
  array.(9).(11) <- "red";
  array.(9).(12) <- "red";
  array.(10).(10) <- "red";
  array.(10).(11) <- "red";
  array.(11).(11) <- "red";
  array.(12).(12) <- "red";
  array.(12).(13) <- "red";
  array.(13).(13) <- "red";
  array.(13).(14) <- "red";
  array.(14).(14) <- "red";
  array.(15).(15) <- "red";
  array.(16).(15) <- "red";
  array.(16).(16) <- "red";
  array.(17).(15) <- "red";
  array.(17).(16) <- "red";
  array.(18).(17) <- "red";
  array.(19).(16) <- "red";
  array.(19).(17) <- "red";
  array.(19).(18) <- "red";
  array.(19).(19) <- "red";
  array

let state_tests = 
  [
    update_board_test "Red 2 player: adding piece 1 in the bottom right" 
      (init_state 2 0 ~-1) "red" (13, 13) (get_piece 0) 
      small_board_with_red_piece;
    update_board_test "Blue 4 player: adding blue piece to initial board in 
      4 player game" (init_state 4 0 ~-1) "blue" (0, 13) (get_piece 0) 
      four_player_blue_board;
    update_board_test "Green 4 player: adding green piece to initial board in 
      4 player game" (init_state 4 0 ~-1) "green" (19, 0) (get_piece 1) 
      big_green_board;
    update_board_test "Adding piece to a more complex board" 
      big_four_player_state "blue" (1, 17) (get_piece 0) winner_board_two_plus;
    update_board_test "Yellow 2 player: adding piece 2 in original orientation 
      to the board at location (0,0) to the initial board with 2 players returns 
      the correct new board" (init_state 2 0 ~-1) "purple" (0, 0) (get_piece 2) 
      small_board_with_purple_piece_2; 
    calculate_score_test "the score of green" big_four_player_state "green" 4; 
    calculate_score_test "the score of a color on an empty board is 0" 
      (init_state 2 0 ~-1) "green" 0;
    calculate_score_test "the score of red on patty's board is 20"
      (create_state pattys_board_1_4p 4 30) "red" 20;
    calculate_score_test "the score of blue on patty's board is 17"
      (create_state pattys_board_1_4p 4 30) "blue" 17;
    winner_test "see if purple wins over red when both have just 2 pieces" 
      two_piece_state "purple";
    winner_test "green is the winner in a 4 player game" 
      big_four_player_state "green";
    winner_test "winner in an empty state is first player" 
      (init_state 2 0 ~-1) "purple";
    is_valid_move_test "tests if a valid_move is true for placing a piece on an 
      empty board" (!init_st) "purple" (0, 0) (get_piece 2) true; 
    is_valid_move_test "valid_move is false for placing a piece on
    a full board" full_state "purple" (0, 0) (get_piece 2) false;
    is_valid_move_test "placing a red piece on bottom right corner of
      small_board_with_purple_piece_2 as initial red move is valid"
      (create_state small_board_with_purple_piece_2 2 1) "red" (12, 12)
      (get_piece 2) true;
    is_valid_move_test "placing a red piece on top right corner of
      small_board_with_purple_piece_2 as initial red move is not valid"
      (create_state small_board_with_purple_piece_2 2 1) "red" (0, 12)
      (get_piece 2) false;
    is_valid_move_test "placing a purple piece with one corner touching another
      purple piece on small_board_with_purple_piece_2 as a non-initial purple
      move is valid" (create_state small_board_with_purple_piece_2 2 2) "purple"
      (2, 2) (get_piece 1) true;
    is_valid_move_test "placing a purple piece that overlaps with a purple piece
      on a non-initial purple move is not valid"
      (create_state small_board_with_purple_piece_2 2 2) "purple" (1, 1)
      (get_piece 3) false;
    is_valid_move_test "piece overlapping with another color is not valid"
      (create_state pattys_board_1_4p 4 16) "purple" (12, 7) (get_piece 6) 
      false;    
    is_valid_move_test "placing a purple piece on
      small_board_with_purple_and_red at (2, 2) is a valid move"
      (create_state small_board_with_purple_and_red 2 2) "purple" (2, 2)
      (get_piece 0) true;
    is_valid_move_test "placing a purple piece on top left corner on empty board
    as initial move is valid move" (init_state 2 0 ~-1) "purple" (0, 0)
      (get_piece 1) true;
    is_valid_move_test "edge touching own color is not valid"
      (create_state pattys_board_1_4p 4 16) "purple" (1, 2) (get_piece 2) false;
    is_valid_move_test "edge touching other color is valid"
      (create_state pattys_board_1_4p 4 16) "purple" (7, 6) (get_piece 6) true;
    is_valid_move_test "there is a corner touching another color but none
    touching your own color is not valid"
      (create_state pattys_board_1_4p 4 16) "purple" (19, 4) (get_piece 6) 
      false;
    is_valid_move_test "putting down a piece that touches your own color at more
    than one corner but no edges is valid"
      (create_state pattys_board_1_4p 4 16) "purple" (7, 6) (get_piece 4) true;
    player_can_move_test "Player can move on an empty board" 
      (init_state 2 0 ~-1) true;
    player_can_move_test "Player can move on small board with purple piece" 
      small_state true;
    player_can_move_test "Player cannot move on a full board small" 
      full_small_state false;
    player_can_move_test "Player cannot move on a full board big" 
      full_state false;
    player_can_move_test 
      "player cannot move if none of the players have pieces left"
      no_pieces_left_state false;
    player_can_move_test "player can move if it is their turn and they have a
      piece that fits but other player doesn't" patty_piece_zero_state true;
    player_can_move_test "player can't move if it is their turn and only the
    other player has a piece that fits" patty_piece_zero_state_2 false;
  ]

let rec string_of_tiles_aux tiles = 
  match tiles with 
  | (x, y) :: t -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ "), " ^ 
                   string_of_tiles_aux t 
  | [] -> ""

let string_of_tiles tiles = 
  "[" ^ string_of_tiles_aux tiles ^ "]"

(** [updated_pattys_board_1_4p] is the updated board after patty has made move
    on [pattys_board_1_4p]. *)
let updated_pattys_board_1_4p =
  let new_board = Array.copy (Array.map Array.copy pattys_board_1_4p) in
  new_board.(6).(10) <- "red";
  new_board.(7).(9) <- "red";
  new_board.(7).(10) <- "red";
  new_board.(7).(11) <- "red";
  new_board.(8).(10) <- "red";
  new_board

let edges_test
    (name: string)
    (board: string array array)
    (color: string)
    (expected_output: (int * int) list) : test =
  name >:: (fun _ -> assert_equal ~cmp: cmp_set_like_lists expected_output
               (List.map TileSet.to_pair (TileSet.elements (edges board color)))
               ~printer:string_of_tiles)

let corners_test
    (name: string)
    (board: string array array)
    (color: string)
    (expected_output: (int * int) list) : test =
  name >:: (fun _ -> assert_equal ~cmp: cmp_set_like_lists expected_output
               (List.map TileSet.to_pair 
                  (TileSet.elements (corners board color)))
               ~printer:string_of_tiles)

let num_tile_compare_test
    (name: string)
    (piece1: int array array)
    (piece2: int array array)
    (expected_output: int) : test =
  name >::
  (fun _ -> assert_equal expected_output (num_tile_compare piece1 piece2)
      ~printer: string_of_int)

let num_tiles_test
    (name: string)
    (piece: int array array)
    (expected_output: int) : test =
  name >:: (fun _ -> assert_equal expected_output (num_tiles piece)
               ~printer: string_of_int)

let num_corners_test
    (name: string)
    (piece: int array array)
    (st: State.t)
    (loc: int * int)
    (expected_output: int) : test =
  name >:: (fun _ -> assert_equal expected_output (num_corners piece st loc)
               ~printer: string_of_int)

let ai_tests =
  [
    edges_test "the list of edges for purple player on board with piece 2 
      at (0,0) is correctly returned" small_board_with_purple_piece_2 "purple"
      [(1, 0); (2, 1); (0, 2); (1, 2)];
    edges_test "the list of edges for red player on board with piece 2 at (0,0)
      is empty" small_board_with_purple_piece_2 "red" [];
    corners_test "the list of corners for purple player on board with piece
      2 at (0, 0) is correctly returned" small_board_with_purple_piece_2
      "purple" [(2, 0); (2, 2)];
    corners_test 
      "the list of corners for red player on board with purple piece 2 at (0,0)
      is empty" small_board_with_purple_piece_2 "red" [];
    num_tile_compare_test "piece 3 is bigger than piece 1" (get_piece 3)
      (get_piece 1) ~-1;
    num_tile_compare_test "piece 1 is smaller than piece 3" (get_piece 1)
      (get_piece 3) 1;
    num_tile_compare_test "piece 2 is the same size as piece 3" (get_piece 2)
      (get_piece 3) 0;
    num_tiles_test "piece 13 has size 5" (get_piece 13) 5;
    num_corners_test "placing purple piece 4 at loc (2, 2) on small board with
      purple piece 2 generates a total of 4 corners for purple" (get_piece 4)
      (create_state small_board_with_purple_piece_2 2 2) (2, 2) 2;
    num_corners_test "placing purple piece 0 at loc (0, 0) on empty board
      generates a total of 1 corner for purple" (get_piece 0)
      (init_state 2 0 ~-1) (0, 0) 1;
    num_corners_test "placing purple piece 4 at loc (2, 2) on small board with 
      purple and red generates a total of 4 corners for purple"
      (get_piece 4) (create_state small_board_with_purple_and_red 2 2) (2, 2) 2;
  ]

let suite =
  "test suite blokus"  >::: List.flatten [
    piece_tests;
    player_tests;
    state_tests;
    ai_tests;
  ]

let _ = run_test_tt_main suite
