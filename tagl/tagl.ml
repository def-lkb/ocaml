external get : 'a -> int = "ml_tagl_get_profinfo" [@@noalloc]
external set : 'a -> int -> bool = "ml_tagl_set_profinfo" [@@noalloc]
