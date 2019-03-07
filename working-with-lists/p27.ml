(* Group the elements of a set into disjoint subsets. *)
let group full_set group_size=
  let sum lst=
    List.fold_left (+) 0 lst
  in
  let append item llst=
    List.map (fun lst->item::lst) llst
  in
  let extract num lst=
    let rec gen cnt lst=
      match lst with
      | hd::tl -> if cnt=0 then [[]] else (append hd (gen (cnt-1) tl))@(if cnt>List.length tl then [] else gen cnt tl)
      | [] -> [[]]
    in
    if num>List.length lst || num<=0 then [] else gen num lst
  in 
  let rec difference a b=
    match a,b with
    | a_hd::a_tl,(b_hd::b_tl as b_lst) -> if a_hd=b_hd then difference a_tl b_tl else a_hd::(difference a_tl b_lst)
    | x,[] -> x
    | _,_ -> [] (* exhaust pattern matching *)
  in let rec spec_map f lst=
  match lst with
  | hd::tl -> (f hd)@(spec_map f tl)
  | [] -> []
  in let rec aux acc full_set group_size=
    match group_size with
    | hd::tl -> if hd>0 then spec_map (fun item-> aux (acc@[item]) (difference full_set item) tl) (extract hd full_set) else aux acc full_set tl
    | [] -> [acc]
  in 
  if sum group_size>List.length full_set then [] else aux [] full_set group_size;;