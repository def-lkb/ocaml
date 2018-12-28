type descriptor = Obj.Tag_descriptor.t

let profinfo_mask = (1 lsl Obj.profinfo_bits ()) - 1
let index x = profinfo_mask land (Obj.Tag_descriptor.hash x)

module Index = struct
  type t = {
    descriptors : (int, descriptor list) Hashtbl.t;
    variants : (int, string list) Hashtbl.t;
  }

  let create () : t = {
    descriptors = Hashtbl.create 17;
    variants = Hashtbl.create 17;
  }

  let register (t : t) = function
    | Obj.Tag_descriptor.Polymorphic_variant_constant name ->
        let i = Obj.Tag_descriptor.hash_variant name in
        begin match Hashtbl.find t.variants i with
        | exception Not_found -> Hashtbl.add t.variants i [name]
        | names -> Hashtbl.replace t.variants i (name :: names)
        end
    | tag ->
        let i = index tag in
        begin match Hashtbl.find t.descriptors i with
        | exception Not_found -> Hashtbl.add t.descriptors i [tag]
        | tags -> Hashtbl.replace t.descriptors i (tag :: tags)
        end

  let register_list t tags =
    List.iter (register t) tags

  let lookup (t : t) i =
    try Hashtbl.find t.descriptors i
    with Not_found -> []

  let lookup_by_profinfo t o =
    lookup t (Obj.get_profinfo (Obj.repr o))

  let lookup_variant (t : t) i =
    try Hashtbl.find t.variants i
    with Not_found -> []
end

module Introspect = struct
  type 'a fields = int * (int -> 'a)

  type dynval =
    | String of string
    (* [String "foo"] = "foo" *)
    | Int of int
    (* [Int 42] = 42    *)
    | Float of float
    (* [Float 12.12] = 12.12 *)
    | Int_or_constant of int * string list
    (* [Int_or_constant (1, ["`Bla"])] = 1 or `Bla *)
    | Tuple of Obj.t fields
    (* [Tuple f] = (f0, f1, f2, ...) *)
    | Array of Obj.t fields
    (* [Array f] = [|f0, f1, f2, ...|] *)
    | Float_array of float fields
    (* [Array f] = [|f0, f1, f2, ...|] *)
    | Record of (string * Obj.t) fields
    (* [Record f] = { fst f0 : snd f0; fst f1 : snd f1; ... } *)
    | Float_record of (string * float) fields
    (* [Float_record f] = { fst f0 : snd f0; fst f1 : snd f1; ... } *)
    | Variant_tuple  of string * Obj.t fields
    | Variant_record of string * (string * Obj.t) fields
    | Polymorphic_variant of string * Obj.t
    | Closure | Lazy | Abstract | Custom | Unknown

  let double_to_wo_size = match Sys.word_size with
    | 64 -> 1
    | _  -> 2

  let fields_of_array arr =
    (Array.length arr, Array.get arr)

  let fields_of_float_array (arr : float array) =
    (Array.length arr, Array.get arr)

  let fields_of_block obj =
    (Obj.size obj, Obj.field obj)

  let map_fields f (size, get) =
    (size, fun i -> f i (get i))

  let find_tag t obj =
    let otag = Obj.tag obj in
    if otag = Obj.int_tag then None
    else
      let osize = Obj.size obj in
      let select = function
        | Obj.Tag_descriptor.Tuple -> true
        | Obj.Tag_descriptor.Array -> true
        | Obj.Tag_descriptor.Polymorphic_variant -> osize = 2
        | Obj.Tag_descriptor.Record fields ->
            otag = 0 && osize = Array.length fields
        | Obj.Tag_descriptor.Float_record fields ->
            otag = Obj.double_array_tag &&
            osize = Array.length fields * double_to_wo_size
        | Obj.Tag_descriptor.Variant_tuple t ->
            otag = t.tag && osize = t.size
        | Obj.Tag_descriptor.Variant_record t ->
            otag = t.tag && osize = Array.length t.fields
        | Obj.Tag_descriptor.Polymorphic_variant_constant _ -> false
        | Obj.Tag_descriptor.Unknown -> false
      in
      List.find_opt select (Index.lookup_by_profinfo t obj)

  let dynval obj =
    let obj = Obj.repr obj in
    if Obj.is_int obj then
      Int (Obj.obj obj)
    else
      let tag = Obj.tag obj in
      if tag <= Obj.last_non_constant_constructor_tag then (
        if tag = 0
        then Tuple (fields_of_block obj)
        else Variant_tuple ("Tag#" ^ string_of_int tag, fields_of_block obj)
      ) else if tag = Obj.string_tag then
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

  let tagged_dynval t obj =
    if Obj.is_int obj then
      let i = (Obj.obj obj : int) in
      match Index.lookup_variant t i with
      | [] -> Int i
      | names -> Int_or_constant (i, names)
    else
      match find_tag t obj with
      | Some (Obj.Tag_descriptor.Tuple) ->
          Tuple (Obj.size obj, Obj.field obj)
      | Some (Obj.Tag_descriptor.Array) ->
          Array (Obj.size obj, Obj.field obj)
      | Some (Obj.Tag_descriptor.Record fields) ->
          Record (map_fields (fun i field -> fields.(i), field)
                    (fields_of_block obj))
      | Some (Obj.Tag_descriptor.Float_record fields) ->
          Float_record (map_fields (fun i field -> fields.(i), field)
                          (fields_of_float_array (Obj.obj obj)))
      | Some (Obj.Tag_descriptor.Variant_tuple t) ->
          Variant_tuple (t.name, fields_of_block obj)
      | Some (Obj.Tag_descriptor.Variant_record t) ->
          let f i field = (t.fields.(i), field) in
          let fields = map_fields f (fields_of_block (Obj.obj obj)) in
          Variant_record (t.name, fields)
      | Some (Obj.Tag_descriptor.Polymorphic_variant) ->
          let name = (Obj.obj (Obj.field obj 0) : int) in
          let payload = Obj.field obj 1 in
          begin match Index.lookup_variant t name with
          | [] -> Polymorphic_variant (string_of_int name, payload)
          | name :: _ -> Polymorphic_variant (name, payload)
          end
      | Some (Obj.Tag_descriptor.Unknown)
      | Some (Obj.Tag_descriptor.Polymorphic_variant_constant _)
      | None -> dynval obj

  (*type outcome =
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
    | Array a ->
        Oarray
          (fmt_fields (var_dump_outcome (depth - 1) width) )
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
    Format.pp_print_flush Format.std_formatter ()*)
end
