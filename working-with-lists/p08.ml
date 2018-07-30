(* Eliminate consecutive duplicates of list elements. *)
let compress lst=
    let rec reduce cur=function
        | [] -> []
        | hd::tl -> if cur=hd then reduce cur tl else hd::(reduce hd tl)
    in match lst with
    | [] -> []
    | hd::tl -> hd::(reduce hd tl);;