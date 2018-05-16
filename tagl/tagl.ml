type t = {
  tag : int;
  size : int;
  constructor : string;
  fields : string list;
}

let hash {tag; size; constructor; fields} =
  let h = Hashtbl.hash tag in
  let h = Hashtbl.seeded_hash h size in
  let h = Hashtbl.seeded_hash h constructor in
  let h = List.fold_left Hashtbl.seeded_hash h fields in
  h

let default = { tag = -1; size = -1; constructor = ""; fields = [] }

external get : 'a -> int = "ml_tagl_get_profinfo" [@@noalloc]
external set : 'a -> int -> bool = "ml_tagl_set_profinfo" [@@noalloc]
external toplevel_library : unit -> t list ref option = "ml_tagl_library"
