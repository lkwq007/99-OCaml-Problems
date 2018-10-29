(* Calculate Euler's totient function φ(m). *)
let phi num=
  let coprime a b=
    let rec aux a b=
      if b=0 then a else aux b (a mod b)
    in (aux a b)=1
  in let rec judge m n count=
       if n<=m then begin if coprime m n then judge m (n+1) (count+1) else judge m (n+1) count end
       else count
  in judge num 1 0;;