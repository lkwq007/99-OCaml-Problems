(* Reverse a list. *)
let rev lst=
    let rec rev_ buf=function
        | [] -> buf
        | hd::tl -> rev_ (hd::buf) tl
    in
    rev_ [] lst;;