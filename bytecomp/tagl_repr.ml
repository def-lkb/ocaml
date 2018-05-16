type t = {
  tag : int;
  size : int;
  constructor : string;
  fields : string list;
}

let library = ref []

let () = Callback.register "tagl_library" library

let make ?(name = "") ?(fields = []) ?(size = -1) tag =
  let result = { tag; size; constructor = name; fields } in
  library := result :: !library;
  result

let hash {tag; size; constructor; fields} =
  let h = Hashtbl.hash tag in
  let h = Hashtbl.seeded_hash h size in
  let h = Hashtbl.seeded_hash h constructor in
  let h = List.fold_left Hashtbl.seeded_hash h fields in
  h

let default = { tag = -1; size = -1; constructor = ""; fields = [] }

let emit_tags () =
  library := List.sort_uniq compare !library;
  default :: !library

let reset_tags () =
  library := []

let flush_tags () =
  let tags = emit_tags () in
  reset_tags ();
  tags
