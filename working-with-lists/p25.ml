(* Generate a random permutation of the elements of a list. *)
let permutation lst=
  let rand_select lst n=
    let rec extract_lst lst k=
      match lst with
      | [] -> raise Not_found
      | hd::tl -> begin if k=0 then (hd,tl) else match (extract_lst tl (k-1)) with (a,b) -> (a,hd::b) end
    in let rec aux lst n=
         if n=0 then [] else 
           match (extract_lst lst (Random.int (List.length lst))) with (a,b) -> a::aux b (n-1)
    in aux lst n
  in rand_select lst (List.length lst);;