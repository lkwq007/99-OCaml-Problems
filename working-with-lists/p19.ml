(* Rotate a list N places to the left. *)
let rotate lst n=
    let rec count acc=function
        | [] -> acc
        | _::tl -> count (acc+1) tl
    in
    let split list n=
        let rec fetch num acc=function
        | [] -> (List.rev acc,[])
        | hd::tl -> if num=1 then (List.rev (hd::acc),tl) else if num<1 then ([],hd::tl) else fetch (num-1) (hd::acc) tl
        in fetch n [] list
    in (if n<0 then split lst (n+count 0 lst) else split lst n) |> (fun (x,y)->y@x);;