(* Extract a given number of randomly selected elements from a list. *)
(* 
   okay, this problem is weird, 
   I have no idea about how to reproduce the given sequence, 
   thus, my solution will simply return randomly selected elements without using ``Random.self_init
*)
let rand_select lst n=
  let rec extract_lst lst k=
    match lst with
    | [] -> raise Not_found
    | hd::tl -> begin if k=0 then (hd,tl) else match (extract_lst tl (k-1)) with (a,b) -> (a,hd::b) end
  in let rec aux lst n=
       if n=0 then [] else 
         match (extract_lst lst (Random.int (List.length lst))) with (a,b) -> a::aux b (n-1)
  in aux lst n;;