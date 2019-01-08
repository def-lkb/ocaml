type frame_pointer

external with_stack : (frame_pointer option -> 'a) -> 'a = "dbgprim_with_stack"
val is_valid : frame_pointer -> bool
val next : frame_pointer -> frame_pointer option
val location : frame_pointer -> int
