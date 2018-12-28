type t = Obj.Tag_descriptor.t =
  | Unknown
  | Tuple
  | Array
  | Record of string array
  | Float_record of string array
  | Variant_tuple  of { tag: int; name: string; size: int }
  | Variant_record of { tag: int; name: string; fields: string array }
  | Polymorphic_variant
  | Polymorphic_variant_constant of string

let mask = (1 lsl Config.profinfo_width) - 1

let index t = mask land (Obj.Tag_descriptor.hash t)

external compiler_tags : unit -> t list ref = "caml_compiler_tags"
let library = compiler_tags ()

let register t = library := t :: !library; t

let make_tuple () = register Tuple

let make_array () = register Array

let make_record fields =
  register (Record fields)

let make_float_record fields =
  register (Float_record fields)

let make_variant tag name size =
  register (Variant_tuple {tag; name; size})

let make_variant_record tag name fields =
  register (Variant_record {tag; name; fields})

let register_polymorphic_variant name =
  ignore (register (Polymorphic_variant_constant name) : t)

let make_polymorphic_variant name =
  register_polymorphic_variant name;
  register Polymorphic_variant

let default = Unknown

let emit_tags () =
  library := List.sort_uniq compare !library;
  default :: !library

let reset_tags () =
  library := []
