type ifloat = private int and t = ifloat

external to_float : ifloat -> float = "%floatofifloat"
external of_float : float -> ifloat = "%ifloatoffloat"
val ( +. )    : ifloat -> ifloat -> ifloat
val ( -. )    : ifloat -> ifloat -> ifloat
val (~+. )    : ifloat -> ifloat
val (~-. )    : ifloat -> ifloat
val ( *. )    : ifloat -> ifloat -> ifloat
val ( /. )    : ifloat -> ifloat -> ifloat
val ( ** )    : ifloat -> ifloat -> ifloat
val sqrt      : ifloat -> ifloat
val exp       : ifloat -> ifloat
val log       : ifloat -> ifloat
val log10     : ifloat -> ifloat
val expm1     : ifloat -> ifloat
val log1p     : ifloat -> ifloat
val cos       : ifloat -> ifloat
val sin       : ifloat -> ifloat
val tan       : ifloat -> ifloat
val acos      : ifloat -> ifloat
val asin      : ifloat -> ifloat
val atan      : ifloat -> ifloat
val atan2     : ifloat -> ifloat -> ifloat
val hypot     : ifloat -> ifloat -> ifloat
val cosh      : ifloat -> ifloat
val sinh      : ifloat -> ifloat
val tanh      : ifloat -> ifloat
val ceil      : ifloat -> ifloat
val floor     : ifloat -> ifloat
val abs_float : ifloat -> ifloat
val copysign  : ifloat -> ifloat -> ifloat
val mod_float : ifloat -> ifloat -> ifloat
val frexp     : ifloat -> ifloat * int
val ldexp     : ifloat -> int -> float
val modf      : ifloat -> ifloat * ifloat
val truncate  : ifloat -> int
val of_int    : int    -> ifloat
val to_int    : ifloat -> int
val classify  : ifloat -> fpclass
val compare   : ifloat -> ifloat -> int
val ( < )     : ifloat -> ifloat -> bool
val ( <= )    : ifloat -> ifloat -> bool
val ( > )     : ifloat -> ifloat -> bool
val ( >= )    : ifloat -> ifloat -> bool
val ( = )     : ifloat -> ifloat -> bool
val ( <> )    : ifloat -> ifloat -> bool

val infinity     : ifloat
val neg_infinity : ifloat
val nan          : ifloat
val max_ifloat   : ifloat
val min_ifloat   : ifloat
val epsilon      : ifloat
