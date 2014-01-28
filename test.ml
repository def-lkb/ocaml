
let f x y z =  
  fst x @ fst y @ snd x @ snd y

let _ = 
  f ([],[3.]) ([0],[])  (*meant 0.*)
(*
let _ = f = true
*)