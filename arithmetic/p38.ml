(* Compare the two methods of calculating Euler's totient function. *)
let timeit f x=
  let t=Unix.gettimeofday () in
  let _=f x in
  Unix.gettimeofday () -. t;;