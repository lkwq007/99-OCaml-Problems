(* Replicate the elements of a list a given number of times. *)
let rec replicate lst num=
    let rec repeat num item tail=
        if num<1 then tail else item::repeat (num-1) item tail
    in match lst with
    | [] -> []
    | hd::tl -> repeat num hd (replicate tl num);;