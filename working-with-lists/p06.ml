(* Find out whether a list is a palindrome. *)
let is_palindrome list=
    let rev lst=
        let rec rev_ buf=function
        | [] -> buf
        | hd::tl -> rev_ (hd::buf) tl
        in
        rev_ [] lst
    in (rev list)=list;;