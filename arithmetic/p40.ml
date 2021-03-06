(* Goldbach's conjecture. *)
let goldbach num=
  let is_prime num=
    let rec aux num n=
      if n*n>num then true else begin if num mod n = 0 then false else aux num (n+1) end
    in if num<2 then false else aux num 2
  in let rec aux n num=
       if n>=num then (0,0) else
       if is_prime n && is_prime (num-n) then (n,num-n) else aux (n+1) num
  in aux 2 num;;