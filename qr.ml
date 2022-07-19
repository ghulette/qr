open Owl

let print_mat ?lbl m =
  Option.iter (fun s -> Format.printf "%s" s) lbl;
  Mat.print ~header:false m

let givens n i j a b =
  let m = Mat.eye n in
  let r = sqrt ((a *. a) +. (b *. b)) in
  let c = a /. r in
  let s = -.b /. r in
  Mat.set m i i c;
  Mat.set m j j c;
  Mat.set m i j s;
  Mat.set m j i (-.s);
  m

let to_zero m i j =
  assert (Mat.col_num m = Mat.row_num m);
  assert (0 <= i && i < Mat.col_num m);
  assert (0 <= j && j < Mat.row_num m);
  assert (i > j);
  let n = Mat.col_num m in
  let a = Mat.get m j j in
  let b = Mat.get m i j in
  givens n i j a b

let () =
  let a =
    Mat.of_arrays [| [| 6.; 5.; 1. |]; [| 5.; 1.; 4. |]; [| 1.; 4.; 3. |] |]
  in
  print_mat ~lbl:"A" a;
  let g = to_zero a 2 0 in
  print_mat ~lbl:"G" g;
  print_mat ~lbl:"GA" (Mat.dot g a)
