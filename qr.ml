open Owl

let givens n i j a b =
  let m = Mat.eye n in
  let r = sqrt ((a *. a) +. (b *. b)) in
  let c = a /. r in
  let s = -.b /. r in
  Format.printf "c=%f\n" c;
  Format.printf "s=%f\n" s;
  Mat.set m i i c;
  Mat.set m j j c;
  Mat.set m i j (-.s);
  Mat.set m j i s;
  m

let () =
  let a1 =
    Mat.of_arrays [| [| 6.; 5.; 0. |]; [| 5.; 1.; 4. |]; [| 0.; 4.; 3. |] |]
  in
  let g1 = givens 3 0 1 (Mat.get a1 0 0) (Mat.get a1 0 1) in
  Format.printf "A1:\n";
  Mat.print ~header:false a1;
  Format.printf "G1:\n";
  Mat.print ~header:false g1;
  let a2 = Mat.dot g1 a1 in
  Format.printf "A2:\n";
  Mat.print ~header:false a2;
  let g2 = givens 3 1 2 (Mat.get a2 1 1) (Mat.get a2 2 1) in
  Format.printf "G2:\n";
  Mat.print ~header:false g2;
  let a3 = Mat.dot g2 a2 in
  Format.printf "A3:\n";
  Mat.print ~header:false a3;
  let q' = Mat.dot g2 g1 in
  let r = Mat.dot q' a1 in
  let q = Mat.transpose q' in
  let a1' = Mat.dot q r in
  Format.printf "A = QR:\n";
  Mat.print ~header:false a1';
  Mat.print ~header:false (Mat.sub a1 a1')
