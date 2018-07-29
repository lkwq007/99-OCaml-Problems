(* Find the k'th element of a list. *)
let rec at index lst=
    match lst with
    | [] -> None
    | head::tail -> let tmp=index-1 in if tmp==0 then Some head else at tmp tail;;