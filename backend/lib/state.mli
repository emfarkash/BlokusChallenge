(** [State] is the representation of the state of the Blokus game.

    This module represents the state of a Blokus game, including the current 
    board, the players, the current turn, the current power ups, etc. *) 

(** The abstract type representing the state of the game *) 
type t

(** The type that represents a power up in the non-classic mode of Blokus *)
type power_up = 
  | Multiplier of int 
  | Skip
  | Penalty of int 

(** Module defining a location on the board to create a set. *)
module S: sig 
  type t = int * int
  val compare: t -> t -> int
end 

(** Set created by [Set.Make(S)] *)
module TileSet:sig
  include Set.S

  val of_pair: int * int -> elt  

  val to_pair: elt -> int * int

  val to_string: t -> string
end

(** [empty] is the empty state state with an empty board, no players, no scores,
    etc. *)
val empty: t

(** [board st] is the board in the current state [st] *)
val board : t -> string array array

(** [players st] is the list of players in state [st] *)
val players : t -> Player.player list

(** [turn_num st] is the current turn number *)
val turn_num : t -> int 

(** [current_turn st] is the color of player whose turn it currently is *)
val current_turn : t -> string

(** [current_player st] is the player whose turn it currently is *)
val current_player: t -> Player.player

(** [classic_mode st] is whether or not the game is in classic mode *)
val classic_mode: t -> bool

(** [powerups st] is the powerups in the the game defined by [st] *)
val powerups: t -> ((int * int) * power_up) list 

(** [oppenents st] are the opponents against the current player *)
val opponents : t -> string list

(** [init_state num_players num_ai ai_level] initializes game state with
    [num_players] players, an empty board (board dimensions: if 2 players or
    less, 14x14, else 20x20) and it being the first player's turn. 
    The non-AI players will be instantiated first. 
    [num_ai] is the number of AI's in the game and [ai_level] is the AI level
    of the AI's.
*)
val init_state : int -> int -> int -> t

(** [init_state_powerups num_players num_ai ai_level] initalizes a game state 
    with powerups that has [num_players] players and [num_ai] AI players
    with [ai_level] difficulty. *)
val init_state_powerups: int -> int -> int -> t 

(** [create_state board num_players turn_num] creates a game state with board
    representation [board], [num_players] players, and turn number [turn_num].
    The players are initialized in their initial state.
    Used for testing purposes. No AI here. *)
val create_state : string array array -> int -> int -> t

(** [create_state_2 board players_lst turn_num] creates a game state with board
    representation [board], list of players [players_lst], and turn number
    [turn_num].
    Used for testing purposes. No AI here. Basically like [create_state] but
    you can specify players. *)
val create_state_2 : string array array -> Player.player list -> int -> t

(** [update_board board color loc piece] updates the blokus board
    with the [piece] added at the specified location [loc] (top left of the
    matrix representation of the [piece]) and with [color]. *)
val update_board: t -> string -> (int * int) -> int array array -> unit

(** [is_valid_move board color loc piece] returns a boolean that represents
    if the [piece] can be placed at [loc] with [color] on [board] under the 
    rules of Blokus. *)
val is_valid_move: t -> string -> (int * int) -> int array array -> bool

(** [move board color loc piece] modifies the blokus board with the [piece]
    added at the specified location [loc] and with [color] if the move is 
    valid. [loc] is the top left of the matrix representing the piece. If the 
    move is not valid, then board is not modified *)
val move: t -> (int * int) -> int array array -> bool

(** [player_can_move board] returns a bool that determines the end of the game 
    given the board and the pieces each player has*)
val player_can_move: t -> bool

(** [calculate_score players] calculates the current score of a given 
    [player]. *)
val calculate_score: t -> string -> int

(** [rotate_piece st p trans] transforms piece [p] by the transformation
    indicated by [trans]*)
val rotate_piece : t -> int array array -> string -> unit

(** [adjacent_corners st loc piece] returns a Set of tiles that share a 
    corner of the [piece] at [loc]. [board] is the current state of the board, 
    [color] is the color piece of the 
    player, [loc] is the specified location, [piece] is the selected piece. *)
val adjacent_corners : t ->  (int * int) -> int array array -> TileSet.t 

(** [winner st] returns the winner of the game. If there's a tie then
    the player with the smaller player number is returned as winner. (e.g. if
    there is a tie between player 1 and player 2 then player 1 is returned as
    winner.) *)
val winner: t -> string

(** [winner_powerups st] is the winner of the game defined by [st]. The winner 
    is calculated by the total number of grids on the board adjusted for any 
    power ups during the game. 

    Requires: [st.classic_mode] is false *)
val winner_powerups: t -> string

(** [next_player st] updates state [st] so that it is the next players turn *)
val next_player: t -> unit

(** [valid_orientations st color x y piece_orientations] returns true if one
    of the piece orientations in [piece_orientations] can be placed onto the
    board in current configuration [st] at position row [x] and col [y]. *)
val valid_orientations: t -> string -> int -> int -> int array array list -> 
  bool 

(** [update_score st tile_set] updates the score of the state by including the 
    score by the new piece defined by the locations in [tile_set]. Requires that 
    [st.classic_mode] is [false] and accounts for [st.powerups] *)
val update_score: t -> TileSet.t -> unit

(** [scores st] is list of the scores scores of the players in [st] in an 
    association list that is in the format (color, score). *)
val scores: t -> (string * int) list

(** [find_powerups st tiles] is a [power_up] list of the power ups found on the 
    locations in [tiles] *)
val find_powerups: t -> (int * int) list -> power_up list 