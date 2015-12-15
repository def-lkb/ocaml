(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Optimization parameters represented as ints indexed by round number. *)
module Int_arg_helper : sig
  type parsed = {
    default : int;
    override : int Ext_types.Int.Map.t;
  }

  val parse : string -> help_text:string -> update:parsed ref -> unit
  val get : key:int -> parsed -> int
end

(** Optimization parameters represented as floats indexed by round number. *)
module Float_arg_helper : sig
  type parsed = {
    default : float;
    override : float Ext_types.Int.Map.t;
  }

  val parse : string -> help_text:string -> update:parsed ref -> unit
  val get : key:int -> parsed -> float
end

type inlining_arguments = {
  inline_call_cost : int option;
  inline_alloc_cost : int option;
  inline_prim_cost : int option;
  inline_branch_cost : int option;
  inline_indirect_cost : int option;
  inline_lifting_benefit : int option;
  branch_inline_factor : float option;
  max_inlining_depth : int option;
  unroll : int option;
  inline_threshold : int option;
  inline_toplevel_threshold : int option;
}

val classic_arguments : inlining_arguments
val o1_arguments : inlining_arguments
val o2_arguments : inlining_arguments
val o3_arguments : inlining_arguments

(** Set all the inlining arguments for a round.
    The default is set if no round is provided. *)
val use_inlining_arguments_set : ?round:int -> inlining_arguments -> unit

val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val compile_only : bool ref
val output_name : string option ref
val include_dirs : string list ref
val no_std_include : bool ref
val print_types : bool ref
val make_archive : bool ref
val debug : bool ref
val fast : bool ref
val link_everything : bool ref
val custom_runtime : bool ref
val no_check_prims : bool ref
val bytecode_compatible_32 : bool ref
val output_c_object : bool ref
val output_complete_object : bool ref
val all_ccopts : string list ref
val classic : bool ref
val nopervasives : bool ref
val open_modules : string list ref
val preprocessor : string option ref
val all_ppx : string list ref
val annotations : bool ref
val binary_annotations : bool ref
val use_threads : bool ref
val use_vmthreads : bool ref
val noassert : bool ref
val verbose : bool ref
val noprompt : bool ref
val nopromptcont : bool ref
val init_file : string option ref
val noinit : bool ref
val use_prims : string ref
val use_runtime : string ref
val principal : bool ref
val real_paths : bool ref
val recursive_types : bool ref
val strict_sequence : bool ref
val strict_formats : bool ref
val applicative_functors : bool ref
val make_runtime : bool ref
val gprofile : bool ref
val c_compiler : string option ref
val no_auto_link : bool ref
val dllpaths : string list ref
val make_package : bool ref
val for_package : string option ref
val error_size : int ref
val float_const_prop : bool ref
val transparent_modules : bool ref
val dump_source : bool ref
val dump_parsetree : bool ref
val dump_typedtree : bool ref
val dump_rawlambda : bool ref
val dump_lambda : bool ref
val dump_rawclambda : bool ref
val dump_clambda : bool ref
val dump_flambda : bool ref
val dump_flambda_let : int option ref
val dump_instr : bool ref
val keep_asm_file : bool ref
val optimize_for_speed : bool ref
val dump_cmm : bool ref
val dump_selection : bool ref
val dump_cse : bool ref
val dump_live : bool ref
val dump_spill : bool ref
val dump_split : bool ref
val dump_interf : bool ref
val dump_prefer : bool ref
val dump_regalloc : bool ref
val dump_reload : bool ref
val dump_scheduling : bool ref
val dump_linear : bool ref
val keep_startup_file : bool ref
val dump_combine : bool ref
val native_code : bool ref
val default_inline_threshold : int
val inline_threshold : Int_arg_helper.parsed ref
val inlining_stats : bool ref
val simplify_rounds : int ref
val default_unroll : int
val unroll : Int_arg_helper.parsed ref
val default_inline_toplevel_threshold : int
val inline_toplevel_threshold : Int_arg_helper.parsed ref
val default_inline_call_cost : int
val default_inline_alloc_cost : int
val default_inline_prim_cost : int
val default_inline_branch_cost : int
val default_inline_indirect_cost : int
val default_inline_lifting_benefit : int
val inline_call_cost : Int_arg_helper.parsed ref
val inline_alloc_cost : Int_arg_helper.parsed ref
val inline_prim_cost : Int_arg_helper.parsed ref
val inline_branch_cost : Int_arg_helper.parsed ref
val inline_indirect_cost : Int_arg_helper.parsed ref
val inline_lifting_benefit : Int_arg_helper.parsed ref
val default_branch_inline_factor : float
val branch_inline_factor : Float_arg_helper.parsed ref
val dont_write_files : bool ref
val std_include_flag : string -> string
val std_include_dir : unit -> string list
val shared : bool ref
val dlcode : bool ref
val pic_code : bool ref
val runtime_variant : string ref
val force_slash : bool ref
val keep_docs : bool ref
val keep_locs : bool ref
val unsafe_string : bool ref
val opaque : bool ref
val print_timings : bool ref
val flambda_invariant_checks : bool ref
val unbox_closures : bool ref
val clambda_checks : bool ref
val default_max_inlining_depth : int
val max_inlining_depth : Int_arg_helper.parsed ref
val inline_recursive_functions : bool ref
val remove_unused_arguments : bool ref
val dump_flambda_verbose : bool ref
val classic_heuristic : bool ref

val all_passes : string list ref
val dumped_pass : string -> bool
val set_dumped_pass : string -> bool -> unit

val parse_color_setting : string -> Misc.Color.setting option
val color : Misc.Color.setting ref
