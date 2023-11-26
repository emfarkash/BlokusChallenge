(** This module handles various operations regarding pieces in a Blokus game. *)

(** [pieces] is the collection of all the matrices representing the pieces,
    listed here in the starting default orientation. Each element of the array
    represents one piece. Each piece is represented by an array, where each
    element represents one row and its elements represent the columns in that
    row. Initial representations here have the piece aligned with the top-most
    row and left-most column of the matrix (but after transformations this may
    change).

    (e.g. a piece represented by [|[|1; 1|]; [|0; 1|]|] has two squares in its
    first row and one square in its second row)

    Requires: matrices must be square.
*)
val pieces : int array array array

(** [get_piece color id] returns the [color]-colored piece associated with [id].
    Requires: [id] is in the range of 0 to 20.  *)
val get_piece : int -> int array array

(** [rotate_cw p] rotates [p] clockwise by 90 degrees. *)
val rotate_cw : int array array -> int array array

(** [rotate_ccw p] rotates [p] counter-clockwise by 90 degrees. *)
val rotate_ccw : int array array -> int array array

(** [reflect_x p] reflects [p] across the horizontal center line of the matrix.
*)
val reflect_x : int array array -> int array array

(** [reflect_y p] reflects [p] across the vertical center line of the matrix. *)
val reflect_y : int array array -> int array array

(** [align_piece piece] aligns the piece [piece] in the matrix such that the 
    neither the first row or first column has only 0's *)
val align_piece : int array array -> int array array

(** [all_orientations ()] is the list of the matrices of each possible 
    orientation of piece [t] *)
val all_orientations : int array array -> int array array list

(** [get_orig_piece orientation_lst] is the piece that is the original 
    orientation of the pieces in [orientation_lst] *)
val get_orig_piece : int array array list -> int array array