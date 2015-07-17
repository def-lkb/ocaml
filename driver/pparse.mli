(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

type error =
  | Cannot_run of string
  | Wrong_pp_magic of string
  | Wrong_file_magic of string

exception Error of error

val preprocess : string -> string
val remove_preprocessed : string -> unit
val file : formatter -> tool_name:string -> string -> (Lexing.lexbuf -> 'a) -> string -> 'a
val apply_rewriters: ?restore:bool -> tool_name:string -> string -> 'a -> 'a
  (** If [restore = true] (the default), cookies set by external rewriters will be
      kept for later calls. *)

val apply_rewriters_str: ?restore:bool -> tool_name:string -> Parsetree.structure -> Parsetree.structure
val apply_rewriters_sig: ?restore:bool -> tool_name:string -> Parsetree.signature -> Parsetree.signature


val report_error : formatter -> error -> unit


val parse_implementation: formatter -> tool_name:string -> string -> Parsetree.structure
val parse_interface: formatter -> tool_name:string -> string -> Parsetree.signature

val write_binary_implementation: Parsetree.structure -> string -> unit
val write_binary_interface: Parsetree.signature -> string -> unit
val read_binary_implementation: string -> Parsetree.structure
val read_binary_interface: string -> Parsetree.signature
