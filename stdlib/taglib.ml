type descriptor = Obj.Tag_descriptor.t

let profinfo_mask = (1 lsl Obj.profinfo_bits ()) - 1
let index x = profinfo_mask land (Obj.Tag_descriptor.hash x)

module Index = struct
  type t = (int, descriptor list) Hashtbl.t

  let create () : t = Hashtbl.create 17

  let register (t : t) tag =
    let i = index tag in
    match Hashtbl.find t i with
    | exception Not_found ->
        Hashtbl.add t i [tag]
    | tags ->
        Hashtbl.replace t i (tag :: tags)

  let register_list t tags =
    List.iter (register t) tags

  let lookup (t : t) i =
    try Hashtbl.find t i
    with Not_found -> []

  let lookup_by_profinfo t o =
    lookup t (Obj.get_profinfo (Obj.repr o))
end

module Introspect = struct
  type fields = int * (int -> Obj.t)

  type dynval =
    | String of string
    | Int    of int
    | Float  of float
    | Float_array of float array
    | Block of int * fields
    | Constructor of string * Obj.t array
    | Record of string * (string * Obj.t) array
    | Closure | Lazy | Abstract | Custom | Unknown

  let find_tag t obj =
    let otag = Obj.tag obj in
    if otag = Obj.int_tag then None
    else
      let osize = Obj.size obj in
      let select {Obj.Tag_descriptor. tag; size} =
        tag = otag && (size = -1 || size = osize)
      in
      List.find_opt select (Index.lookup_by_profinfo t obj)

  let dynval obj =
    let obj = Obj.repr obj in
    if Obj.is_int obj then
      Int (Obj.obj obj)
    else
      let tag = Obj.tag obj in
      if tag <= Obj.last_non_constant_constructor_tag then
        Block (tag, (Obj.size obj, Obj.field obj))
      else if tag = Obj.string_tag then
        String (Obj.obj obj)
      else if tag = Obj.double_tag then
        Float (Obj.obj obj)
      else if tag = Obj.double_array_tag then
        Float_array (Obj.obj obj)
      else if tag = Obj.closure_tag then
        Closure
      else if tag = Obj.lazy_tag then
        Lazy
      else if tag = Obj.abstract_tag then
        Abstract
      else if tag = Obj.custom_tag then
        Custom
      else
        Unknown

  let fields_of_array arr =
    (Array.length arr, Array.get arr)

  type outcome =
    | Ostring of string
    | Ofloat of float
    | Oint of int
    | Oarray of outcome list
    | Oconstr of string * outcome list
    | Orecord of (string * outcome) list
    | Oellipsis
    | Oother of string

  let rec format_outcome ppf = function
    | Ostring x -> Format.fprintf ppf "%S" x
    | Ofloat x -> Format.fprintf ppf "%f" x
    | Oint x -> Format.fprintf ppf "%d" x
    | Oarray xs ->
        let format_elements ppf xs =
          List.iter (Format.fprintf ppf "%a;" format_outcome) xs
        in
        Format.fprintf ppf "@[<hov>[|%a|]@]" format_elements xs
    | Oconstr (name, []) ->
        Format.fprintf ppf "%s" name
    | Oconstr (name, xs) ->
        let format_elements ppf xs =
          List.iter (Format.fprintf ppf "%a;" format_outcome) xs
        in
        Format.fprintf ppf "%s (@[<hov>%a@])" name format_elements xs
    | Orecord xs ->
        let format_element ppf (k,v) =
          Format.fprintf ppf "%s: %a;" k format_outcome v
        in
        let format_elements ppf xs =
          List.iter (format_element ppf) xs
        in
        Format.fprintf ppf "{@[<hov>%a@]}" format_elements xs
    | Oellipsis ->
        Format.fprintf ppf "..."
    | Oother str ->
        Format.fprintf ppf "%s" str

  let rec var_dump_outcome depth width obj =
    let fmt_fields f (size, field) =
      let list =
        ref (if size > width then [Oellipsis] else [])
      in
      for i = min width size - 1 downto 0 do
        list := f (field i) :: !list
      done;
      !list
    in
    match dynval obj with
    | String str ->
        Ostring str
    | Float flt ->
        Ofloat flt
    | Int i ->
        Oint i
    | Float_array a ->
        Oarray
          (fmt_fields (fun flt -> Ofloat flt) (fields_of_array a))
    | Block (tag, fields) ->
        Oconstr (
          "Block." ^ string_of_int tag,
          fmt_fields (var_dump_outcome (depth - 1) width) fields
        )
    | Constructor (name, fields) ->
        Oconstr (
          name,
          fmt_fields (var_dump_outcome (depth - 1) width)
            (fields_of_array fields)
        )
    | Record (name, fields) ->
        let dump_field (name, v) =
          (name, var_dump_outcome (depth - 1) width v)
        in
        Oconstr (
          name,
          [Orecord (Array.to_list (Array.map dump_field fields))]
        )
    | Closure  -> Oother "<closure>"
    | Lazy     -> Oother "<lazy>"
    | Abstract -> Oother "<abstract>"
    | Custom   -> Oother "<custom>"
    | Unknown  -> Oother "<unknown>"

  let var_dump_outcome ?(depth=5) ?(width=80) v =
    var_dump_outcome depth width (Obj.repr v)

  let var_dump v =
    format_outcome Format.std_formatter (var_dump_outcome v);
    Format.pp_print_flush Format.std_formatter ()
end
