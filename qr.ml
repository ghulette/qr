open Owl

module List = struct
  include List

  let prod xs ys =
    List.(concat_map (fun x -> concat_map (fun y -> [ (x, y) ]) ys) xs)

  let range n = List.init n Fun.id

  let fold_left1 f = function
    | [] -> raise (Invalid_argument "fold_left1")
    | x :: xs -> List.fold_left f x xs
end

let is_square m = Mat.col_num m = Mat.row_num m

let lower_tri_ijs n =
  let ns = List.range n in
  List.prod ns ns |> List.filter (fun (i, j) -> i > j)

module Approx = struct
  let is_zero x = Float.abs x < 1E-10

  let is_upper_tri m =
    is_square m
    &&
    let n = Mat.col_num m in
    lower_tri_ijs n |> List.for_all (fun (i, j) -> is_zero (Mat.get m i j))

  let is_eye m =
    is_square m
    &&
    let n = Mat.col_num m in
    Mat.(sub m (eye n)) |> Mat.for_all is_zero

  let is_orthogonal m =
    is_eye Mat.(dot m (transpose m)) && is_eye Mat.(dot (transpose m) m)

  let equal m1 m2 = Mat.sub m1 m2 |> Mat.for_all is_zero
end

let givens n i j a b =
  let r = sqrt ((a *. a) +. (b *. b)) in
  let c = a /. r in
  let s = -.b /. r in
  let m = Mat.eye n in
  Mat.set m i i c;
  Mat.set m j j c;
  Mat.set m i j s;
  Mat.set m j i (-.s);
  m

let to_zero m i j =
  assert (is_square m);
  let n = Mat.col_num m in
  assert (0 <= i && i < n);
  assert (0 <= j && j < n);
  assert (i > j);
  let a = Mat.get m j j in
  let b = Mat.get m i j in
  if Approx.is_zero b then Mat.eye n else givens n i j a b

let qr a =
  assert (is_square a);
  let n = Mat.col_num a in
  let rec loop r gs = function
    | [] -> (r, gs)
    | (i, j) :: ijs ->
        let g = to_zero r i j in
        loop (Mat.dot g r) (g :: gs) ijs
  in
  let r, gs = loop a [] (lower_tri_ijs n) in
  let q = List.fold_left1 Mat.dot gs |> Mat.transpose in
  (q, r)

let () =
  (* Example from Wikipedia: *)
  (* let a = Mat.of_array [| 6.; 5.; 1.; 5.; 1.; 4.; 0.; 4.; 3. |] 3 3 in *)

  (* "random" matrix *)
  let a = Mat.(sub_scalar (mul_scalar (uniform 5 5) 200.) 100.) in
  Format.printf "A:";
  Mat.print a;
  let q, r = qr a in
  Format.printf "\nQ:";
  Mat.print q;
  assert (Approx.is_orthogonal q);
  Format.printf "\nR:";
  Mat.print r;
  assert (Approx.is_upper_tri r);
  let qr = Mat.dot q r in
  assert (Approx.equal a qr);
  Format.printf "\nQR - A:";
  Mat.print (Mat.sub qr a)
