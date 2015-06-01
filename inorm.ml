open Ifloat

let eval_A i j = of_float 1. /. of_int ((i+j)*(i+j+1)/2+i+1)

let eval_A_times_u u v =
  let n = Array.length v - 1 in
  for i = 0 to  n do
    let vi = ref (of_float 0.) in
      for j = 0 to n do vi := !vi +. eval_A i j *. u.(j) done;
      v.(i) <- !vi
  done

let eval_At_times_u u v =
  let n = Array.length v -1 in
  for i = 0 to n do
    let vi = ref (of_float 0.) in
      for j = 0 to n do vi := !vi +. eval_A j i *. u.(j) done;
      v.(i) <- !vi
  done

let eval_AtA_times_u u v =
  let w = Array.make (Array.length u) (of_float 0.) in
  eval_A_times_u u w; eval_At_times_u w v


let res =
  let n = try int_of_string(Array.get Sys.argv 1) with _ -> 2000 in
  let u = Array.make n (of_float 1.0)  and  v = Array.make n (of_float 0.0) in
  for i = 0 to 9 do
    eval_AtA_times_u u v; eval_AtA_times_u v u
  done;

  let vv = ref (of_float 0.0)  and  vBv = ref (of_float 0.0) in
  for i=0 to n-1 do
    vv := !vv +. v.(i) *. v.(i);
    vBv := !vBv +. u.(i) *. v.(i)
  done;
  (sqrt(!vBv /. !vv))

let () =
  if res > of_float 1.0 && res < of_float 2.0 then print_endline "OK";
  print_float (to_float res);
  print_newline ();
  Printf.printf "%0.9f\n" (to_float res);
  Gc.print_stat stdout
