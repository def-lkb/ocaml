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

  type approx = Obj.Tag_descriptor.approx

  type dynval =
    | String of string
    (* [String "foo"] = "foo" *)
    | Float of float
    (* [Float 12.12] = 12.12 *)
    | Char of char
    (* [Char 'c'] = 'x' *)
    | Int_or_constant of int * string list
    (* [Int_or_constant (1, ["`Bla"])] = 1 or `Bla *)
    | Constant of string list
    (* [Constant ["`Bla"]] = `Bla *)
    | Array of (approx * Obj.t) fields
    (* [Array f] = [|f0, f1, f2, ...|] *)
    | Tuple of { name: string; fields: (approx * Obj.t) fields }
    (* [Tuple f] = (f0, f1, f2, ...) *)
    | Record of { name: string; fields: (string * (approx * Obj.t)) fields }
    (* [Record f] = { fst f0 : snd f0; fst f1 : snd f1; ... } *)
    | Polymorphic_variant of string * Obj.t
    | Closure | Lazy | Abstract | Custom | Unknown

  let double_to_wo_shift = match Sys.word_size with
    | 64 -> 0
    | _  -> 1

  let fields_of_block f obj =
    if Obj.tag obj = Obj.double_array_tag then
      (Obj.size obj lsr double_to_wo_shift,
       fun i -> f i (Obj.repr (Obj.double_field obj i)))
    else
      (Obj.size obj, fun i -> f i (Obj.field obj i))

  let map_fields f (size, get) =
    (size, fun i -> f i (get i))

  let find_tag t obj =
    let otag = Obj.tag obj in
    if otag = Obj.int_tag then None
    else
      let osize = Obj.size obj in
      let select = function
        | Obj.Tag_descriptor.Array _ -> true
        | Obj.Tag_descriptor.Polymorphic_variant -> osize = 2
        | Obj.Tag_descriptor.Tuple t ->
            otag = t.tag && osize = Array.length t.fields
        | Obj.Tag_descriptor.Record t ->
            otag = t.tag &&
            let len = Array.length t.fields in
            let len =
              if otag = Obj.double_array_tag
              then len lsl double_to_wo_shift else len
            in
            osize = len
        | Obj.Tag_descriptor.Polymorphic_variant_constant _ -> false
        | Obj.Tag_descriptor.Unknown -> false
      in
      List.find_opt select (Index.lookup_by_profinfo t obj)

  let no_approx' (_ : int) (obj : Obj.t) = (Obj.Tag_descriptor.Any, obj)

  let dynval obj =
    let obj = Obj.repr obj in
    if Obj.is_int obj then
      Int_or_constant (Obj.obj obj, [])
    else
      let tag = Obj.tag obj in
      if tag <= Obj.last_non_constant_constructor_tag then (
        if tag = 0
        then Tuple { name = ""; fields = fields_of_block no_approx' obj }
        else Tuple { name = "Tag#" ^ string_of_int tag;
                     fields = fields_of_block no_approx' obj }
      ) else if tag = Obj.string_tag then
        String (Obj.obj obj)
      else if tag = Obj.double_tag then
        Float (Obj.obj obj)
      else if tag = Obj.double_array_tag then
        Array (fields_of_block no_approx' obj)
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

  let no_approx (obj : Obj.t) = (Obj.Tag_descriptor.Any, obj)

  let tagged_dynval t (approx, obj) =
    if Obj.is_int obj then
      let i = (Obj.obj obj : int) in
      match approx with
       | Obj.Tag_descriptor.Any ->
           Int_or_constant (i, Index.lookup_variant t i)
       | Obj.Tag_descriptor.Int ->
           Int_or_constant (i, [])
       | Obj.Tag_descriptor.Char ->
           (try Char (Char.chr i) with _ -> Int_or_constant (i, []))
       | Obj.Tag_descriptor.Constants names ->
           if i >= 0 && i < Array.length names
           then Constant [names.(i)]
           else Int_or_constant (i, [])
       | Obj.Tag_descriptor.Polymorphic_variants ->
           begin match Index.lookup_variant t i with
           | [] -> Int_or_constant (i, [])
           | names -> Constant names
           end
    else
      match find_tag t obj with
      | Some (Obj.Tag_descriptor.Array approx ) ->
          Array (fields_of_block (fun _ obj -> approx, obj) obj)
      | Some (Obj.Tag_descriptor.Record {name; fields}) ->
          let get_field i obj =
            let fname, fapprox = fields.(i) in
            (fname, (fapprox, obj))
          in
          Record { name; fields = fields_of_block get_field obj }
      | Some (Obj.Tag_descriptor.Tuple {name; fields}) ->
          let get_field i obj = (fields.(i), obj) in
          Tuple {name; fields = fields_of_block get_field obj}
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
