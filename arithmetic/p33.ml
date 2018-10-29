(* Determine whether two positive integer numbers are coprime. *)
let coprime a b=
  let rec aux a b=
    if b=0 then a else aux b (a mod b)
  in (aux a b)=1;;