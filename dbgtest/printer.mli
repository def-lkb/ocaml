val fmt : ('a, Format.formatter, unit) format -> Format.formatter -> 'a

type 'a t = Format.formatter -> 'a -> unit

val list : 'a t -> 'a list t
val placeholder : 'a t

val position : Lexing.position t
val location : Location.t t
val debug_event : Instruct.debug_event t
