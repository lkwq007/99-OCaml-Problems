(* Extract a slice from a list. *)
let slice lst lower upper=
  let rec select n low up=function
    | [] -> []
    | hd::tl -> if n>=low && n<=up then hd::select (n+1) low up tl else select (n+1) low up tl
  in select 0 lower upper lst;;