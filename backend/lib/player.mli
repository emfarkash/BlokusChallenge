(** Representation of a player in a Blokus game. Includes the player's pieces, 
    the player's color, and whether the player is an AI in addition to AI level
    if the player is an AI. *)

(** [S] is the module definition for a piece.*) 
module S : sig 
  type t = int array array 
  val compare : t -> t -> int 
  (* val of_piece : int array array -> t  *)
end

(** [PieceSet] is the implementation of module [S] as a set. *)
module PieceSet : sig
  include Set.S
  val to_piece : elt -> int array array
  val of_piece : int array array -> elt
end

(** [player] is the abstract type representing the player in the game.  *)
type player 

(** [init_player is_ai color ai_level] is a player with a full set of Blockus 
    pieces, the ai_opponent field equal to [is_ai] and color field equal to 
    [color]. [ai_level] is the level of the AI (if the player is not an AI
    then this field's value does not matter). *)
val init_player : bool -> string -> int -> player

(** [create_player is_ai color ai_level pieces_lst can_move] is a player with
    the set of Blokus pieces represented by [pieces_lst]. If [is_ai] is true 
    then the player is an AI with level [level]. The player's color is [color] 
    and [can_move] is true if it is this player's turn in the game.  
    Used for testing purposes. *)
val create_player : bool -> string -> int -> PieceSet.t -> bool -> player

(** [update_pieces p pieces] updates [p.pieces] to [pieces] *)
val update_pieces: PieceSet.t -> player -> unit

(** [remove_piece piece player] modifies the pieces set so that the piece 
    [piece] is removed from the pieces of the player.. 
    Requires: [player] has [pieces] within their set of pieces, and both 
    [player] and [pieces] are valid. *)
val remove_piece : int array array -> player -> unit

(** [replace_piece old_piece new_piece player] modifies the pieces set such that
    [old_piece] is replaced by [new_piece] *)
val replace_piece : int array array -> int array array -> player -> unit

(** [ai_opponent player] returns the [ai_opponent] associated with the player 
    [player].
    Requires: [player] is a valid player. *)
val ai_opponent : player -> bool

(** [ai_level player] returns the level for the AI for player [player]
    Requires: [player.ai_opponent] is true *)
val ai_level: player -> int 

(** [color player] returns the [color] associated with the player [player].
    Requires: [player] is a valid player. *)
val color : player -> string

(** [can_move_f player] returns whether or not the player [player] can make a
    move.
    Requires: [player] is a valid player. *)
val can_move_f : player -> bool

(** [pieces player] returns the [pieces] set associated with the player 
    [player].
    Requires: [player] is a valid player. *)
val pieces : player -> PieceSet.t