(* Split a list into two parts; the length of the first part is given. *)
let split lst n=
    let rec fetch num acc=function
        | [] -> (List.rev acc,[])
        | hd::tl -> if num=1 then (List.rev (hd::acc),tl) else if num<1 then ([],hd::tl) else fetch (num-1) (hd::acc) tl
    in fetch n [] lst;; 