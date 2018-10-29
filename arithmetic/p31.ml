(* Determine whether a given integer number is prime. *)
let is_prime num=
  let rec aux num n=
    if 2*n>num then true else begin if num mod n = 0 then false else aux num (n+1) end
  in if num<2 then false else aux num 2;;
