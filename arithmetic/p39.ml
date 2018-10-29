(* A list of prime numbers. *)
let rec all_primes a b=
  let is_prime num=
    let rec aux num n=
      if n*n>num then true else begin if num mod n = 0 then false else aux num (n+1) end
    in if num<2 then false else aux num 2
  in 
  if a>b then [] else if is_prime a then a::(all_primes (a+1) b) else all_primes (a+1) b;;