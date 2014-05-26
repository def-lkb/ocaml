(*let f x y = 
   if x > y then [x]  (* missing "else [y]" *)
*)

let f =
   print_string (Printf.sprintf "%d" 4) 4
