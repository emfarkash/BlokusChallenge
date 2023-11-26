
(** The implementation of the module definition for a piece.*) 
module S = struct  
  type t = int array array

  let compare (piece_1 : t) (piece_2 : t)= 
    let is_equal = ref 0 in
    if(Array.length piece_1 = Array.length piece_2) then begin
      let length = Array.length piece_1 in 
      for x = 0 to length - 1 do
        for y = 0 to length - 1 do
          if (piece_1.(x).(y) <> piece_2.(x).(y))
          then is_equal := -1
        done
      done;
    end 
    else is_equal := 1;
    !is_equal
end

(** Implementation of [PieceSet] module. *)
module PieceSet = struct
  include Set.Make(S)

  let remove (elt : elt) (s : t) = 
    filter (fun p -> p <> elt) s 

  let to_piece (elt: elt): int array array = 
    elt

  let of_piece (p: int array array): elt =
    p
end

(** [player] is the implementation of the abstract type. Players are given a
    color-ID [color] (a string representing the color of the player’s pieces).
    Players will each have a complete set of Blokus pieces, represented by a set
    [pieces]. They are also either a an AI or a real player (represented by a
    boolean [ai_opponent] where true indicates that the player is an AI and
    false indicates the opposite) and either can or cannot make more moves
    (represented by a boolean [can_move] where true indicates that the player
    can move and false indicates the opposite). *)
type player = {
  ai_opponent : bool;
  ai_level : int;
  mutable pieces : PieceSet.t;
  color : string;
  mutable can_move : bool;
} 

(** [init_player is_ai color] is a player with a full set of Blokus 
    pieces, the ai_opponent field equal to [is_ai] and color field equal to 
    [color]. *)
let init_player ai color level: player = 
  {ai_opponent =  ai; ai_level = level; 
   pieces = PieceSet.of_list (Array.to_list Piece.pieces)
  ;color = color; can_move = true}

let create_player ai color level pieces_lst can_move =
  {ai_opponent = ai; ai_level = level;
   pieces = pieces_lst; color = color; can_move = can_move}

(** [remove_piece piece player] modifies the pieces set so that the piece 
    [piece] is removed from the pieces of the player. *)
let remove_piece piece player = 
  player.pieces <- PieceSet.remove piece player.pieces

let can_move_f player = 
  player.can_move

let pieces player = 
  player.pieces

let ai_opponent player = 
  player.ai_opponent

let ai_level player =
  player.ai_level

let update_pieces pieces player =
  player.pieces <- pieces

let color player =
  player.color

let replace_piece old_piece new_piece p =
  let new_set = PieceSet.remove old_piece p.pieces |> PieceSet.add new_piece in
  p.pieces <- new_set
