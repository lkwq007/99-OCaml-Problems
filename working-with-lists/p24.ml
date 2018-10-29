(* Lotto: Draw N different random numbers from the set 1..M. *)
let lotto_select n m=
  let rec range a b=
    if a>b then [] else a::(range (a+1) b)
  in
  let rand_select lst n=
    let rec extract_lst lst k=
      match lst with
      | [] -> raise Not_found
      | hd::tl -> begin if k=0 then (hd,tl) else match (extract_lst tl (k-1)) with (a,b) -> (a,hd::b) end
    in let rec aux lst n=
         if n=0 then [] else 
           match (extract_lst lst (Random.int (List.length lst))) with (a,b) -> a::aux b (n-1)
    in aux lst n
  in rand_select (range 1 m) n;;