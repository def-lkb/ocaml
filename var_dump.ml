type reflection =
  | Int of int
  | Float of float
  | Float_array of float array
  | String of string
  | Tuple of Obj.t array
  | Constructor of string * Obj.t array
  | Inline_record of string * string list * Obj.t array
  | Record of string list * Obj.t array
  | Float_record of string list * float array
  | Perdu of Obj.t

let mask = (1 lsl Obj.profinfo_bits () - 1)
let key tag = Obj.Tag_descriptor.hash tag land mask

let index = lazy (
  let tags = Obj.Tag_descriptor.read_self_descriptors () in
  let table = Hashtbl.create 131 in
  List.iter (fun tag ->
      let key = key tag in
      match Hashtbl.find table key with
      | exception Not_found -> Hashtbl.add table key [tag]
      | tags -> Hashtbl.replace table key (tag :: tags)
    ) tags;
  table
)

let array_of_fields obj =
  Array.init (Obj.size obj) (Obj.field obj)

external compiler_tags
  : unit -> Obj.Tag_descriptor.t list ref
  = "caml_compiler_tags"

let compiler_tags = compiler_tags ()

let lookup profinfo =
  let lazy index = index in
  try Hashtbl.find index profinfo
  with Not_found ->
    List.filter (fun tag -> key tag = profinfo) !compiler_tags

let reflect_with_profinfo tag obj =
  let open Obj.Tag_descriptor in
  let size = Obj.size obj in
  let tags = lookup (Obj.get_profinfo obj) in
  let tags =
    let check a b = a = -1 || a = b in
    let pred t = check t.tag tag && check t.size size in
    List.filter pred tags
  in
  if tag = Obj.double_array_tag then
    let obj : float array = Obj.obj obj in
    match tags with
    | {fields} :: _ when List.length fields = Array.length obj ->
        Float_record (fields, obj)
    | _ -> Float_array obj
  else
    match tags with
    | {tag = 0; fields = []; constructor = ""} :: _ ->
        Tuple (array_of_fields obj)
    | {fields = []; constructor} :: _ ->
        Constructor (constructor, array_of_fields obj)
    | {fields; constructor = ""} :: _ ->
        Record (fields, array_of_fields obj)
    | {fields; constructor} :: _ ->
        Inline_record (constructor, fields, array_of_fields obj)
    | [] -> Perdu obj

let reflect obj =
  let tag = Obj.tag obj in
  if tag = Obj.int_tag
  then Int (Obj.obj obj)
  else if
    (tag <= Obj.last_non_constant_constructor_tag &&
     tag >= Obj.first_non_constant_constructor_tag) ||
    (tag = Obj.double_array_tag)
  then reflect_with_profinfo tag obj
  else if tag = Obj.double_tag then
    Float (Obj.obj obj)
  else if tag = Obj.string_tag then
    String (Obj.obj obj)
  else Perdu obj

open Format

let print_floats ppf f =
  Array.iter (fprintf ppf "%f;@ ") f

let rec print_record ppf (keys, values) =
  List.iteri (fun i key ->
      fprintf ppf "@[%s = %a@];@ " key pp_hum values.(i))
    keys

and print_float_record ppf (keys, values) =
  List.iteri (fun i key ->
      fprintf ppf "@[%s = %f@];@ " key values.(i))
    keys

and pp_hum ppf obj =
  match reflect obj with
  | String s ->
      fprintf ppf "%S" s
  | Int i ->
      fprintf ppf "%d" i
  | Float f ->
      fprintf ppf "%f" f
  | Float_array f ->
      fprintf ppf "[|@[<hv>%a@]|]" print_floats f
  | Tuple t ->
      fprintf ppf "(@[<hv>%a@])"
        (fun ppf a ->
           Array.iteri (fun i o ->
               if i > 0 then fprintf ppf ",@ ";
               pp_hum ppf o
             ) a
        )
        t
  | Constructor ("::", t) ->
      fprintf ppf "[@[<hv>%a@]]" print_list t
  | Constructor (name, t) ->
      fprintf ppf "%s(@[<hv>%a@])" name
        (fun ppf a ->
           Array.iteri (fun i o ->
               if i > 0 then fprintf ppf ",@ ";
               pp_hum ppf o
             ) a
        )
        t
  | Record (keys, values) ->
      fprintf ppf "{@[<hv>%a@]}" print_record (keys, values)

  | Float_record (keys, values) ->
      fprintf ppf "{@[<hv>%a@]}" print_float_record (keys, values)

  | Inline_record (name, keys, values) ->
      fprintf ppf "%s{@[<hv>%a@]}" name print_record (keys, values)

  | Perdu _ ->
      fprintf ppf "<perdu>"

and print_list ppf x =
  fprintf ppf "@[%a" pp_hum x.(0);
  match reflect x.(1) with
  | Int 0 -> fprintf ppf "@]"
  | Constructor ("::", t) ->
      fprintf ppf ";@]@ ";
      print_list ppf t
  | _ ->
    fprintf ppf " . @]@ ";
    pp_hum ppf x.(1)

let var_dump obj =
  fprintf Format.err_formatter "%a\n%!" pp_hum (Obj.repr obj);

