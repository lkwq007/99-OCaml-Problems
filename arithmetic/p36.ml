(* Determine the prime factors of a given positive integer (2). *)
let factors num=
  let rec aux num fact acc=
    if fact=num then [(fact,acc+1)] else if fact>num then if acc=0 then [] else [(fact,acc)] else 
    if num mod fact=0 then (aux (num/fact) fact (acc+1)) else if acc=0 then (aux num (fact+1) 0) else (fact,acc)::(aux num (fact+1) 0)
  in aux num 2 0;;