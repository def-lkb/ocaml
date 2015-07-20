(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree
open Compenv

(* Save a .cmp(i) file *)

let save_cmp ~sourcefile ~outputprefix ~modulename ast fn =
  let cmp_infos = { Cmp_format.
                    cmp_source_file = sourcefile;
                    cmp_output_prefix = outputprefix;
                    cmp_module_name = modulename;
                    cmp_input_name = !Location.input_name;
                    cmp_content = ast
                  }
  in
  Cmp_format.write_cmp_infos cmp_infos fn

(* Compile a .mli file *)

(* Keep in sync with the copy in compile.ml *)

let tool_name = "ocamlopt"

let process_signature ppf sourcefile outputprefix initial_env modulename ast =
  if Clflags.don't_stop_at Clflags.Typing then
    begin
      if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
      if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;
      let tsg = Typemod.type_interface initial_env ast in
      if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
      let sg = tsg.sig_type in
      if !Clflags.print_types then
        Printtyp.wrap_printing_env initial_env (fun () ->
            fprintf std_formatter "%a@."
              Printtyp.signature (Typemod.simplify_signature sg));
      ignore (Includemod.signatures initial_env sg sg);
      Typecore.force_delayed_checks ();
      Warnings.check_fatal ();
      if not !Clflags.print_types then begin
        let sg = Env.save_signature sg modulename (outputprefix ^ ".cmi") in
        Typemod.save_signature modulename tsg outputprefix sourcefile
          initial_env sg ;
      end
    end

let interface ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  let ast = Pparse.parse_interface ~tool_name ppf sourcefile in
  if !Clflags.binary_ast then
    save_cmp ~sourcefile ~outputprefix ~modulename
      (Cmp_format.Cmp_interface ast) (outputprefix ^ ".cmpi");
  process_signature ppf sourcefile outputprefix initial_env modulename ast

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x
let (+++) (x, y) f = (x, f y)

let process_structure
    ppf sourcefile outputprefix initial_env modulename cmxfile objfile ast =
  let transl_step ast =
    try
      ast
      ++ Translmod.transl_store_implementation modulename
      +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
      +++ Simplif.simplify_lambda
      +++ print_if ppf Clflags.dump_lambda Printlambda.lambda
      ++ Asmgen.compile_implementation outputprefix ppf;
      Compilenv.save_unit_info cmxfile;
      Warnings.check_fatal ();
    with exn ->
      remove_file objfile;
      remove_file cmxfile;
      raise exn
  in
  let typing_step ast =
    if Clflags.don't_stop_at Clflags.Typing then
      ast
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation sourcefile outputprefix modulename initial_env
      ++ print_if ppf Clflags.dump_typedtree
        Printtyped.implementation_with_coercion
      ++ fun x ->
        if Clflags.don't_stop_at Clflags.Compiling && not !Clflags.print_types then
          transl_step x
        else
          Warnings.check_fatal ();
    else ()
  in
  Misc.try_finally
    (fun () -> typing_step ast)
    (fun () -> Stypes.dump (Some (outputprefix ^ ".annot")))


let implementation ppf sourcefile outputprefix =
  Compmisc.init_path true;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env() in
  Compilenv.reset ?packname:!Clflags.for_package modulename;
  let cmxfile = outputprefix ^ ".cmx" in
  let objfile = outputprefix ^ ext_obj in
  let ast = Pparse.parse_implementation ~tool_name ppf sourcefile in
  if !Clflags.binary_ast then
    save_cmp ~sourcefile ~outputprefix ~modulename
      (Cmp_format.Cmp_implementation ast) (outputprefix ^ ".cmp");
  process_structure
    ppf sourcefile outputprefix initial_env modulename cmxfile objfile ast

let cmp_file ppf cmp_infos =
  let open Cmp_format in
  match cmp_infos.cmp_content with
  | Cmp_implementation str ->
      Compmisc.init_path true;
      Location.input_name := cmp_infos.cmp_input_name;
      Env.set_unit_name cmp_infos.cmp_module_name;
      let initial_env = Compmisc.initial_env() in
      Compilenv.reset ?packname:!Clflags.for_package cmp_infos.cmp_module_name;
      let cmxfile = cmp_infos.cmp_output_prefix ^ ".cmx" in
      let objfile = cmp_infos.cmp_output_prefix ^ ext_obj in
      process_structure ppf
        cmp_infos.cmp_source_file
        cmp_infos.cmp_output_prefix
        initial_env
        cmp_infos.cmp_module_name
        cmxfile
        objfile
        str
  | Cmp_interface sg ->
      Compmisc.init_path false;
      Location.input_name := cmp_infos.cmp_input_name;
      Env.set_unit_name cmp_infos.cmp_module_name;
      let initial_env = Compmisc.initial_env() in
      process_signature ppf
        cmp_infos.cmp_source_file
        cmp_infos.cmp_output_prefix
        initial_env
        cmp_infos.cmp_module_name
        sg


let c_file name =
  let output_name = !Clflags.output_name in
  if Ccomp.compile_file ~output_name name <> 0 then exit 2
