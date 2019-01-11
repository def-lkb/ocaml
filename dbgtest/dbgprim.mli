type frame_pointer

external with_stack : (frame_pointer option -> 'a) -> 'a = "dbgprim_with_stack"
val is_valid : frame_pointer -> bool
val next : frame_pointer -> frame_pointer option
val peek : frame_pointer -> int -> Obj.t option
val peek_stack : frame_pointer -> int -> Obj.t option
val peek_heap : frame_pointer -> int -> Obj.t option
val peek_rec : frame_pointer -> int -> Obj.t option
val location : frame_pointer -> int

external log : 'a -> unit = "dbgprim_log"
