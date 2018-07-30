(* Run-length encoding of a list. *)
let encode lst=
    let rec count acc num=function
        | [] -> if acc=[] then [] else [num,List.hd acc]
        | hd::tl -> if acc=[] || (List.hd acc)=hd then count [hd] (num+1) tl else (num,List.hd acc)::(count [hd] 1 tl)
    in count [] 0 lst;;