(* Find the number of elements of a list. *)
let length lst=
  let rec count num=function
    | [] -> num
    | _::tail -> count (num+1) tail 
  in
  count 0 lst;;