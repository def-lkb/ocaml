type _ term =
          | Int : int -> int term
          | Add : (int -> int -> int) term
          | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n    -> n                 (* a = int *)
  | Add      -> (fun x y -> x+y)  (* a = int -> int -> int *)
  | App(f,x) -> (eval f) (eval x)

(* -easytpe above currently produces an error due to incomplete treatment of GADTs
     Error: the previous branches produce values of type [int] 
       but this branch has type [int -> int -> int].
*)

let rec sum = function
   | [] -> 0
   | a::l -> a +. (sum l)

(* Here we get a good error message thanks to -easytype *)