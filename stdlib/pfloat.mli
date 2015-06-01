type pfloat = private int and t = pfloat

external to_float : pfloat -> float = "%floatofpfloat"
external of_float : float -> pfloat = "%pfloatoffloat"
val ( +. )    : pfloat -> pfloat -> pfloat
val ( -. )    : pfloat -> pfloat -> pfloat
val (~+. )    : pfloat -> pfloat
val (~-. )    : pfloat -> pfloat
val ( *. )    : pfloat -> pfloat -> pfloat
val ( /. )    : pfloat -> pfloat -> pfloat
val ( ** )    : pfloat -> pfloat -> pfloat
val sqrt      : pfloat -> pfloat
val exp       : pfloat -> pfloat
val log       : pfloat -> pfloat
val log10     : pfloat -> pfloat
val expm1     : pfloat -> pfloat
val log1p     : pfloat -> pfloat
val cos       : pfloat -> pfloat
val sin       : pfloat -> pfloat
val tan       : pfloat -> pfloat
val acos      : pfloat -> pfloat
val asin      : pfloat -> pfloat
val atan      : pfloat -> pfloat
val atan2     : pfloat -> pfloat -> pfloat
val hypot     : pfloat -> pfloat -> pfloat
val cosh      : pfloat -> pfloat
val sinh      : pfloat -> pfloat
val tanh      : pfloat -> pfloat
val ceil      : pfloat -> pfloat
val floor     : pfloat -> pfloat
val abs_float : pfloat -> pfloat
val copysign  : pfloat -> pfloat -> pfloat
val mod_float : pfloat -> pfloat -> pfloat
val frexp     : pfloat -> pfloat * int
val ldexp     : pfloat -> int -> float
val modf      : pfloat -> pfloat * pfloat
val truncate  : pfloat -> int
val of_int    : int    -> pfloat
val to_int    : pfloat -> int
val classify  : pfloat -> fpclass
val compare   : pfloat -> pfloat -> int
val ( < )     : pfloat -> pfloat -> bool
val ( <= )    : pfloat -> pfloat -> bool
val ( > )     : pfloat -> pfloat -> bool
val ( >= )    : pfloat -> pfloat -> bool
val ( = )     : pfloat -> pfloat -> bool
val ( <> )    : pfloat -> pfloat -> bool

val infinity     : pfloat
val neg_infinity : pfloat
val nan          : pfloat
val max_pfloat   : pfloat
val min_pfloat   : pfloat
val epsilon      : pfloat
