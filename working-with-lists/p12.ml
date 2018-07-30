(* Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)
type 'a rle =
    | One of 'a
    | Many of int * 'a;;
let rec decode lst=
    let rec expand num elem tail=
        if num>1 then elem::expand (num-1) elem tail else elem::tail
    in match lst with
    | [] -> []
    | (One x)::tl -> x::decode tl
    | (Many (n,x))::tl -> expand n x (decode tl);;