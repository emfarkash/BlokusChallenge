(* This module is the support for AI players as well as six different levels of 
   difficulty for AI players that range from easy to hard. *)

(** [move st] is the updated game state after an AI player tries to make a move.
    Requires: [State.current_player st |> Player.ai_opponent] is [true] *)
val move: State.t -> State.t

(**[edges board color] is the TileSet that contains all of the edges that 
   belong to a player with color [color] on the board [board].
   Requires: Board is not empty.
*)
val edges: string array array -> string -> State.TileSet.t

(**[corners board color] gets all tiles that are corners of [color] on the board
   [board].
   Requires: Board is not empty. *)
val corners: string array array -> string -> State.TileSet.t

(**[num_tile_compare piece1 piece2] returns -1 if [piece1] has more units than
   [piece2], 1 if [piece1] has less units than [piece2], or 0 if [piece1] has 
   the same number of units as [piece2].*)
val num_tile_compare: int array array -> int array array -> int

(**[num_tiles piece] is the number of tiles that a piece [piece] takes up as in 
   the number of units that are 1 in the matrix. *)
val num_tiles: int array array -> int

(**[num_corners piece st loc] is the number of free corners that putting piece
   [piece] in location [loc] generates for the ai player.*)
val num_corners: int array array -> State.t -> int * int -> int