(* Run-length encoding of a list (direct solution). *)
type 'a rle =
    | One of 'a
    | Many of int * 'a;;
let encode lst=
    let seal item num=
        if num>1 then [Many (num,List.hd item)] else if num=1 then [One (List.hd item)] else []
    in
    let rec count acc num=function
        | [] -> seal acc num
        | hd::tl -> if acc=[] || (List.hd acc)=hd then count [hd] (num+1) tl else (seal acc num)@(count [hd] 1 tl)
    in count [] 0 lst;;