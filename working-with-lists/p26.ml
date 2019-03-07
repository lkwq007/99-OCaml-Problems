(* Generate the combinations of K distinct objects chosen from the N elements of a list.  *)
let extract num lst=
  let append item llst=
    List.map (fun lst->item::lst) llst
  in
  let rec gen cnt lst=
    match lst with
    | hd::tl -> if cnt=0 then [[]] else (append hd (gen (cnt-1) tl))@(if cnt>List.length tl then [] else gen cnt tl)
    | [] -> [[]]
  in
  if num>List.length lst || num<=0 then [] else gen num lst;;