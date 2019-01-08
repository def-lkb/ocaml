type frame_pointer

external with_stack : (frame_pointer option -> 'a) -> 'a = "dbgprim_with_stack"
external is_valid : frame_pointer -> bool             = "dbgprim_is_valid"
external next : frame_pointer -> frame_pointer option = "dbgprim_next"
external location : frame_pointer -> int              = "dbgprim_location"
