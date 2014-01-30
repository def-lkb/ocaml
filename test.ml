let rec f x = 
  match x with
  | 0 -> 0.0
  | _ -> print_float (f (x-1))
  | _ -> print_int (f (x-1))

(*
let _ =
  let x = ref [] in
  x := [3];
  x := [4.0]
*)


(*
let _ =
  let y = ref 2 in
  if x > 0
    y = 3;
  print_int y
  *)