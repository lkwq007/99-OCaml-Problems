(* Pack consecutive duplicates of list elements into sublists. *)
let pack lst=
    let rec combine acc=function
        | [] -> if acc=[] then [] else [acc]
        | hd::tl -> if acc=[] || hd=(List.hd acc) then combine (hd::acc) tl else acc::(combine [hd] tl)
    in combine [] lst;;