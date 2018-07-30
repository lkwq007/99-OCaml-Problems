(* Insert an element at a given position into a list. *)
let insert_at item k lst=
    let rec insert x n=function
        | [] -> if n>=0 then [x] else []
        | hd::tl -> if n=0 then x::hd::tl else hd::insert x (n-1) tl
    in insert item k lst;;