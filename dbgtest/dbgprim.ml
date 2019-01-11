type frame_pointer

external with_stack : (frame_pointer option -> 'a) -> 'a = "dbgprim_with_stack"
external is_valid : frame_pointer -> bool             = "dbgprim_is_valid"
external next : frame_pointer -> frame_pointer option = "dbgprim_next"
external peek : frame_pointer -> int -> Obj.t option  = "dbgprim_peek"
external location : frame_pointer -> int              = "dbgprim_location"

external peek_stack : frame_pointer -> int -> Obj.t option = "dbgprim_peek_stack"
external peek_heap  : frame_pointer -> int -> Obj.t option = "dbgprim_peek_heap"
external peek_rec   : frame_pointer -> int -> Obj.t option = "dbgprim_peek_rec"

external log : 'a -> unit = "dbgprim_log"
