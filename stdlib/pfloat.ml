(* Make the compiler think its immediate *)
type pfloat = private int
type t = pfloat

external to_float : pfloat -> float = "%floatofpfloat"
external of_float : float -> pfloat = "%pfloatoffloat"

let ( +. ) a b = of_float (to_float a +. to_float b)
let ( -. ) a b = of_float (to_float a -. to_float b)
let (~+. ) a   = of_float (~+. (to_float a))
let (~-. ) a   = of_float (~-. (to_float a))
let ( *. ) a b = of_float (to_float a *. to_float b)
let ( /. ) a b = of_float (to_float a /. to_float b)
let ( ** ) a b = of_float (to_float a ** to_float b)
let sqrt  a = of_float (sqrt (to_float a))
let exp   a = of_float (exp (to_float a))
let log   a = of_float (log (to_float a))
let log10 a = of_float (log10 (to_float a))
let expm1 a = of_float (expm1 (to_float a))
let log1p a = of_float (log1p (to_float a))
let cos   a = of_float (cos (to_float a))
let sin   a = of_float (sin (to_float a))
let tan   a = of_float (tan (to_float a))
let acos  a = of_float (acos (to_float a))
let asin  a = of_float (asin (to_float a))
let atan  a = of_float (atan (to_float a))
let atan2 a b = of_float (atan2 (to_float a) (to_float b))
let hypot a b = of_float (hypot (to_float a) (to_float b))
let cosh  a = of_float (cosh (to_float a))
let sinh  a = of_float (sinh (to_float a))
let tanh  a = of_float (tanh (to_float a))
let ceil  a = of_float (ceil (to_float a))
let floor a = of_float (floor (to_float a))
let abs_float a = of_float (abs_float (to_float a))
let copysign a b = of_float (copysign (to_float a) (to_float b))
let mod_float a b = of_float (mod_float (to_float a) (to_float b))
let frexp i = let a, b = frexp (to_float i) in of_float a, b
let ldexp a b = ldexp (to_float a) b
let modf i = let a, b = modf (to_float i) in of_float a, of_float b
let truncate i = truncate (to_float i)
let of_int i = of_float (float_of_int i)
let to_int i = int_of_float (to_float i)

let infinity = of_float infinity
let neg_infinity = of_float neg_infinity
let nan = of_float nan
let max_pfloat = of_float max_float
let min_pfloat = of_float min_float
let epsilon = of_float epsilon_float

let classify i = classify_float (to_float i)

let compare a b = compare (to_float a) (to_float b)
let ( <  ) a b = (to_float a) <  (to_float b)
let ( <= ) a b = (to_float a) <= (to_float b)
let ( >  ) a b = (to_float a) >  (to_float b)
let ( >= ) a b = (to_float a) >= (to_float b)
let ( =  ) a b = (to_float a) =  (to_float b)
let ( <> ) a b = (to_float a) <> (to_float b)
