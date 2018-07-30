(* Flatten a nested list structure *)
(* There is no nested list type in OCaml, so we need to define one
   first. A node of a nested list is either an element, or a list of
   nodes. *)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;
let rec flatten=function
    | [] -> []
    | (One x)::tl -> x::(flatten tl);
    | (Many x)::tl -> (flatten x)@(flatten tl);;