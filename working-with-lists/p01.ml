(* Write a function last : 'a list -> 'a option that returns the last element of a list. *)
let rec last lst = 
  match lst with
  | [] -> None
  | head::[] -> Some head
  | head::tail -> last tail;;