(* Find the last but one (last and penultimate) elements of a list. *)
let rec last_two = function
    | [] -> None
    | [x] -> None
    | [x;y] -> Some (x,y)
    | _::tail -> last_two tail;;