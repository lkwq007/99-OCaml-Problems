(* Create a list containing all integers within a given range. *)
let range a b=
    let rec range_op a b next cmp=
        if cmp a b then a::range_op (next a) b next cmp else []
    in if a<b then range_op a b (succ) (<=) else range_op a b (pred) (>=);;