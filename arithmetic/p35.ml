(* Determine the prime factors of a given positive integer. *)
let factors num=
  let rec aux num fact=
    if fact=num then [num] else if fact>num then [] else
    if num mod fact=0 then fact::(aux (num/fact) 2) else (aux num (fact+1))
  in aux num 2;;