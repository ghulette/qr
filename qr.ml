open Owl

let is_approx_zero x = Float.abs x < 0.00001
let is_square m = Mat.col_num m = Mat.row_num m

let fold_left1 f = function
  | [] -> raise (Invalid_argument "fold_left1")
  | x :: xs -> List.fold_left f x xs

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
  assert (is_square m);
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

let qr a =
  assert (is_square a);
  let r = ref a in
  let gs = ref [] in
  for i = 0 to Mat.row_num a - 1 do
    for j = 0 to Mat.col_num a - 1 do
      if i > j then begin
        let g = to_zero !r i j in
        r := Mat.dot g !r;
        gs := g :: !gs
      end
    done
  done;
  let q = Mat.(fold_left1 Mat.dot !gs |> transpose) in
  (q, !r)

let () =
  (* let a = Mat.of_array [| 6.; 5.; 1.; 5.; 1.; 4.; 10.; 4.; 3. |] 3 3 in *)
  let a = Mat.magic 5 in
  test_to_zero a;
  let q, r = qr a in
  Mat.print q;
  Mat.print r;
  Mat.print a;
  Mat.(dot q r |> print)
