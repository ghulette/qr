open Owl

let is_approx_zero x = Float.abs x < 0.00001

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
  let n = Mat.col_num m in
  assert (0 <= i && i < n);
  assert (0 <= j && j < n);
  assert (i > j);
  let a = Mat.get m j j in
  let b = Mat.get m i j in
  if is_approx_zero b then Mat.eye n else givens n i j a b

let test_to_zero a =
  Mat.print a;
  for i = 0 to Mat.row_num a - 1 do
    for j = 0 to i - 1 do
      Format.printf "\n\n=== Zero (%d,%d) ===\n" i j;
      let g = to_zero a i j in
      Format.printf "\nG";
      Mat.print g;
      let ga = Mat.dot g a in
      Format.printf "\nGA";
      Mat.print ga;
      assert (Mat.get ga i j |> is_approx_zero)
    done
  done

let () =
  Mat.of_array [| 6.; 5.; 1.; 5.; 1.; 4.; 0.; 4.; 3. |] 3 3 |> test_to_zero;
  Mat.magic 4 |> test_to_zero
