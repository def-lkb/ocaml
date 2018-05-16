type t = Obj.Tag_descriptor.t = {
  tag : int;
  size : int;
  constructor : string;
  fields : string list;
}

let mask = (1 lsl Config.profinfo_width) - 1

let index t = mask land (Obj.Tag_descriptor.hash t)

external compiler_tags : unit -> t list ref = "caml_compiler_tags"
let library = compiler_tags ()

let make ?(name = "") ?(fields = []) ?(size = -1) tag =
  let result = { tag; size; constructor = name; fields } in
  library := result :: !library;
  result

let default = { tag = -1; size = -1; constructor = ""; fields = [] }

let emit_tags () =
  library := List.sort_uniq compare !library;
  default :: !library

let reset_tags () =
  library := []
