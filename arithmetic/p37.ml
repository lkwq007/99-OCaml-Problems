(* Calculate Euler's totient function φ(m) (improved). *)
let phi_improved num=
  let factors num=
    let rec aux num fact acc=
      if fact=num then [(fact,acc+1)] else if fact>num then if acc=0 then [] else [(fact,acc)] else 
      if num mod fact=0 then (aux (num/fact) fact (acc+1)) else if acc=0 then (aux num (fact+1) 0) else (fact,acc)::(aux num (fact+1) 0)
    in aux num 2 0
  in let rec pow acc base n=
       if n<=0 then acc else pow (acc*base) base (n-1)
  in let rec result acc=function
      | [] -> acc
      | hd::tl -> begin match hd with (p,m) -> result (acc*(p-1)*(pow 1 p (m-1))) tl end
  in result 1 (factors num);;