(* Determine the greatest common divisor of two positive integer numbers. *)
let gcd a b=
  let rec aux a b=
    if b=0 then a else aux b (a mod b)
  in aux a b;;