let pieces : int array array array = (* t array*)
  [|
    (* 0 *) [|[|1|]|];
    (* 1 *) [|[|1; 1|]; [|0; 0|]|];
    (* 2 *) [|[|1; 1|]; [|0; 1|]|];
    (* 3 *) [|[|1; 1; 1|]; [|0; 0; 0|]; [|0; 0; 0|]|];
    (* 4 *) [|[|1; 1|]; [|1; 1|]|];
    (* 5 *) [|[|0; 1; 0|]; [|1; 1; 1|]; [|0; 0; 0|]|];
    (* 6 *) [|[|1; 1; 1; 1|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|];
    (* 7 *) [|[|0; 0; 1|]; [|1; 1; 1|]; [|0; 0; 0|]|];
    (* 8 *) [|[|0; 1; 1|]; [|1; 1; 0|]; [|0; 0; 0|]|];
    (* 9 *) [|[|1; 0; 0; 0|]; [|1; 1; 1; 1|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|];
    (* 10 *) [|[|0; 1; 0|]; [|0; 1; 0|]; [|1; 1; 1|]|];
    (* 11 *) [|[|1; 0; 0|]; [|1; 0; 0|]; [|1; 1; 1|]|];
    (* 12 *) [|[|0; 1; 1; 1|]; [|1; 1; 0; 0|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|];
    (* 13 *) [|[|0; 0; 1|]; [|1; 1; 1|]; [|1; 0; 0|]|];
    (* 14 *) [|[|1; 0; 0; 0; 0|]; [|1; 0; 0; 0; 0|]; [|1; 0; 0; 0; 0|]; 
               [|1; 0; 0; 0; 0|]; [|1; 0; 0; 0; 0|]|];
    (* 15 *) [|[|1; 0; 0|]; [|1; 1; 0|]; [|1; 1; 0|]|];
    (* 16 *) [|[|0; 1; 1|]; [|1; 1; 0|]; [|1; 0; 0|]|];
    (* 17 *) [|[|1; 1; 0|]; [|1; 0; 0|]; [|1; 1; 0|]|];
    (* 18 *) [|[|0; 1; 1|]; [|1; 1; 0|]; [|0; 1; 0|]|];
    (* 19 *) [|[|0; 1; 0|]; [|1; 1; 1|]; [|0; 1; 0|]|];
    (* 20 *) [|[|0; 1; 0; 0|]; [|1; 1; 1; 1|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|]
  |]

let get_piece id =
  pieces.(id)

(** [matrix_center max_row max_col] is the center coordinate (center row,
    center column) of the matrix with maximum row index [max_row] and maximum
    column index [max_col].
    (e.g. the center of matrix [|[|1; 1|]; [|1; 1|]|] is (0.5, 0.5), and its
    [max_row] is 1 and [max_col] is 1). *)
let matrix_center max_row max_col =
  ((float_of_int max_row)/. 2., (float_of_int max_col) /. 2.)

(** [cw_transform row col max_row max_col matrix value] adds
    the new element of [value] in the correct position in [matrix] after that
    point has been rotated clockwise 90 degrees from its original position in
    its old matrix. [row] is the row that the element was in in its old matrix
    and [col] is its old column. [max_row] is the maximum row index in the
    matrix, and [max_col] is the maximum column index in the matrix. *)
let cw_transform row col max_row max_col matrix (value : int) =
  let position =
    (col,
     int_of_float(
       -.((float_of_int row) -. fst(matrix_center max_row max_col)) +.
       fst(matrix_center max_row max_col))) in
  matrix.(fst(position)).(snd(position)) <- value

(** [transform_row row row_idx new_matrix trans] transforms the [rox_idx]th row
    of an old matrix into a [new_matrix] using transformation [trans]. [row] is
    an array of the values in the row we want to transform.
    (e.g. To transform the 2nd row (e.g. [|1; 0|]) of some old matrix by
    rotation clockwise, use [transform_row 2 new_matrix cw_transform]).
    Requires: dimensions of [new_matrix] must be the same as the dimensions
    of the matrix where [row] came from. *)
let transform_row row (row_idx : int) new_matrix trans =
  for iter = 0 to (Array.length row) - 1
  do
    trans row_idx iter ((Array.length new_matrix) - 1)
      ((Array.length new_matrix.(0)) - 1) new_matrix row.(iter)
  done

(** [transform_matrix p trans] transforms the entire [matrix] using
    transformation [trans]. *)
let transform_matrix matrix trans =
  let new_matrix =
    Array.make_matrix (Array.length matrix) (Array.length matrix.(0)) 0 in
  for row_idx = 0 to (Array.length matrix) - 1
  do
    transform_row matrix.(row_idx) row_idx new_matrix trans
  done; new_matrix

let rotate_cw p : int array array =
  transform_matrix p cw_transform
(* {p with matrix = transform_matrix p.matrix cw_transform} *)

let rotate_ccw p =
  rotate_cw p 
  |> rotate_cw 
  |> rotate_cw

(** [refl_x_transform row col max_row max_col matrix value] adds the new element
    of [value] added to [matrix] in the correct position after 
    that point has been reflected vertically across the center horizontal axis 
    of the matrix. [row] is the row that the element was in in its old matrix
    and [col] is its old column. [max_row] is the maximum row index in the
    matrix, and [max_col] is the maximum column index in the matrix. *)
let refl_x_transform row col max_row max_col matrix (value : int) =
  let position =
    (int_of_float
       (-.((float_of_int row) -. fst(matrix_center max_row max_col)) +.
        fst(matrix_center max_row max_col)), col)
  in
  matrix.(fst(position)).(snd(position)) <- value

let reflect_x p =
  transform_matrix p refl_x_transform

(** [refl_y_transform row col max_row max_col matrix value] returns [matrix] 
    with the new element of [value] added to it in the correct position after 
    that point has been reflected horizontally across the center vertical axis
    of the matrix. [row] is the row that the element was in in its old matrix
    and [col] is its old column. [max_row] is the maximum row index in the
    matrix, and [max_col] is the maximum column index in the matrix. *)
let refl_y_transform row col max_row max_col matrix (value : int) =
  let position =
    (row, int_of_float
       (-.(float_of_int col -. snd(matrix_center max_row max_col)) +.
        snd(matrix_center max_row max_col)))
  in
  matrix.(fst(position)).(snd(position)) <- value (*; matrix*)

let reflect_y p =
  transform_matrix p refl_y_transform

let not_zero elt = 
  if elt = 0 then false
  else true

let rec rotate_top p = 
  let piece_list = Array.to_list p in
  match piece_list with
  | [] -> [||]
  | h :: t -> begin
      if Array.exists not_zero h then p
      else rotate_top (Array.of_list (t @ [h]))
    end

let rec get_column (p : int array array) (col : int) (acc: int list): int list = 
  let piece_list = Array.to_list p in
  match piece_list with
  | [] -> acc
  | h :: t -> get_column (Array.of_list t) col (acc @ [h.(col)])

let transpose (piece: int array array) : int array array = 
  let length = Array.length piece in 
  let new_piece = Array.make_matrix length length 0 in 
  for i = 0 to length - 1 do 
    let new_row = ref [| |] in 
    new_row := Array.append (get_column piece i [] 
                             |> Array.of_list) !new_row;
    new_piece.(i) <- !new_row
  done;
  new_piece

let rotate_left (piece: int array array) : int array array  =  
  transpose piece 
  |> rotate_top 
  |> transpose 

let align_piece (piece : int array array) : int array array = 
  piece
  |> rotate_top
  |> rotate_left

let all_orientations piece = 
  let orientations = [] in
  let ori_1 = rotate_cw piece in
  let orientations = ori_1 :: orientations in 
  let ori_1r = reflect_y ori_1 in
  let orientations = ori_1r :: orientations in 
  let ori_2 = rotate_cw ori_1 in 
  let orientations = ori_2 :: orientations in 
  let ori_2r = reflect_y ori_2 in
  let orientations = ori_2r :: orientations in 
  let ori_3 = rotate_cw ori_2 in
  let orientations = ori_3 :: orientations in 
  let ori_3r = reflect_y ori_3 in
  let orientations = ori_3r :: orientations in 
  let ori_4 = rotate_cw ori_3 in
  let orientations = ori_4 :: orientations in 
  let ori_4r = reflect_y ori_4 in
  let orientations = ori_4r :: orientations in 
  orientations

let rec get_orig_piece (orientation_lst: int array array list)  = 
  match orientation_lst with
  | [] -> failwith "Not a valid piece"
  | h :: t -> if List.exists (fun p -> p = h) (Array.to_list pieces) then h
    else get_orig_piece t