(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Operations on internal representations of values *)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_int : t -> bool = "%obj_is_int"
let [@inline always] is_block a = not (is_int a)
external tag : t -> int = "caml_obj_tag"
external set_tag : t -> int -> unit = "caml_obj_set_tag"
external size : t -> int = "%obj_size"
external reachable_words : t -> int = "caml_obj_reachable_words"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external floatarray_get : floatarray -> int -> float = "caml_floatarray_get"
external floatarray_set :
    floatarray -> int -> float -> unit = "caml_floatarray_set"
let [@inline always] double_field x i = floatarray_get (obj x : floatarray) i
let [@inline always] set_double_field x i v =
  floatarray_set (obj x : floatarray) i v
external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"
external add_offset : t -> Int32.t -> t = "caml_obj_add_offset"

external profinfo_bits : unit -> int = "caml_obj_profinfo_bits" [@@noalloc]
external get_profinfo : t -> int = "caml_obj_get_profinfo" [@@noalloc]
external set_profinfo : t -> int -> bool = "caml_obj_set_profinfo"

let marshal (obj : t) =
  Marshal.to_bytes obj []
let unmarshal str pos =
  (Marshal.from_bytes str pos, pos + Marshal.total_size str pos)

let first_non_constant_constructor_tag = 0
let last_non_constant_constructor_tag = 245

let lazy_tag = 246
let closure_tag = 247
let object_tag = 248
let infix_tag = 249
let forward_tag = 250

let no_scan_tag = 251

let abstract_tag = 251
let string_tag = 252
let double_tag = 253
let double_array_tag = 254
let custom_tag = 255
let final_tag = custom_tag


let int_tag = 1000
let out_of_heap_tag = 1001
let unaligned_tag = 1002

let extension_constructor x =
  let x = repr x in
  let slot =
    if (is_block x) && (tag x) <> object_tag && (size x) >= 1 then field x 0
    else x
  in
  let name =
    if (is_block slot) && (tag slot) = object_tag then field slot 0
    else invalid_arg "Obj.extension_constructor"
  in
    if (tag name) = string_tag then (obj slot : extension_constructor)
    else invalid_arg "Obj.extension_constructor"

let [@inline always] extension_name (slot : extension_constructor) =
  (obj (field (repr slot) 0) : string)

let [@inline always] extension_id (slot : extension_constructor) =
  (obj (field (repr slot) 1) : int)

module Ephemeron = struct
  type obj_t = t

  type t (** ephemeron *)

  external create: int -> t = "caml_ephe_create"

  let length x = size(repr x) - 2

  external get_key: t -> int -> obj_t option = "caml_ephe_get_key"
  external get_key_copy: t -> int -> obj_t option = "caml_ephe_get_key_copy"
  external set_key: t -> int -> obj_t -> unit = "caml_ephe_set_key"
  external unset_key: t -> int -> unit = "caml_ephe_unset_key"
  external check_key: t -> int -> bool = "caml_ephe_check_key"
  external blit_key : t -> int -> t -> int -> int -> unit
    = "caml_ephe_blit_key"

  external get_data: t -> obj_t option = "caml_ephe_get_data"
  external get_data_copy: t -> obj_t option = "caml_ephe_get_data_copy"
  external set_data: t -> obj_t -> unit = "caml_ephe_set_data"
  external unset_data: t -> unit = "caml_ephe_unset_data"
  external check_data: t -> bool = "caml_ephe_check_data"
  external blit_data : t -> t -> unit = "caml_ephe_blit_data"


end

module Tag_descriptor = struct
  type t =
    | Unknown
    | Tuple
    | Array
    | Record of string array
    | Float_record of string array
    | Variant_tuple  of { tag: int; name: string; size: int }
    | Variant_record of { tag: int; name: string; fields: string array }
    | Polymorphic_variant
    | Polymorphic_variant_constant of string

  (* Copied from Hashtbl, to avoid introducing a dependency *)
  external seeded_hash_param :
    int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]

  let hash_combine seed v = seeded_hash_param 10 100 seed v
  let hash_array h strings = Array.fold_left hash_combine h strings

  let hash_variant s =
    let accu = ref 0 in
    for i = 0 to String.length s - 1 do
      accu := 223 * !accu + Char.code s.[i]
    done;
    (* reduce to 31 bits *)
    accu := !accu land (1 lsl 31 - 1);
    (* make it signed for 64 bits architectures *)
    if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

  let hash = function
    | Unknown -> 0
    | Tuple -> 1
    | Array -> 2
    | Record fields -> hash_array 3 fields
    | Float_record fields -> hash_array 4 fields
    | Variant_tuple { tag; name; size } ->
        hash_combine (hash_combine (hash_combine 5 tag) name) size
    | Variant_record { tag; name; fields } ->
        hash_array (hash_combine (hash_combine 6 tag) name) fields
    | Polymorphic_variant -> 7
    | Polymorphic_variant_constant _ -> 0

  external read_self_descriptors : unit -> t list =
    "caml_read_tag_section"
end
