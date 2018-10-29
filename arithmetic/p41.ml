(* A list of Goldbach compositions. *)
let rec goldbach_list a b=
  let goldbach num=
    let is_prime num=
      let rec aux num n=
        if n*n>num then true else begin if num mod n = 0 then false else aux num (n+1) end
      in if num<2 then false else aux num 2
    in let rec aux n num=
         if n>=num then (0,0) else
         if is_prime n && is_prime (num-n) then (n,num-n) else aux (n+1) num
    in aux 2 num
  in
  if a mod 2 <> 0 then goldbach_list (a+1) b 
  else if a=2 then (goldbach_list (a+2) b)
  else if a>b then [] 
  else (a,goldbach a)::(goldbach_list (a+2) b);;
let goldbach_limit a b limit=
  let rec aux limit=function
    | [] -> []
    | ((x,(a,b)) as hd)::tl -> if a>limit && b>limit then hd::(aux limit tl) else (aux limit tl)
  in aux limit (goldbach_list a b);;