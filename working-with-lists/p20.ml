(* Remove the K'th element from a list. *)
let remove_at k lst=
    let rec remove k n=function
        | [] -> []
        | hd::tl -> if k=n then tl else hd::remove k (n+1) tl
    in remove k 0 lst;;