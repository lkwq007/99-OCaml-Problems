(* Drop every N'th element from a list. *)
let drop lst n=
    let rec real_drop n num=function
        | [] -> []
        | hd::tl -> if num=1 then real_drop n n tl else hd::real_drop n (num-1) tl
    in real_drop n n lst;;