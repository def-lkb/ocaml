type cmp_content =
  | Cmp_interface of Parsetree.signature
  | Cmp_implementation of Parsetree.structure

type error =
  Wrong_magic of string

exception Error of error

type cmp_infos = {
  cmp_content : cmp_content;
  cmp_output_prefix : string;
  cmp_source_file : string;
  cmp_module_name : string;
  cmp_input_name : string;
  cmp_comments : (string * Location.t) list;
}

let write_cmp_infos (infos : cmp_infos) fn =
  let oc = open_out_bin fn in
  output_string oc Config.cmp_magic_number;
  output_value oc infos;
  close_out oc

let read_cmp_infos fn : cmp_infos =
  let magic = Config.cmp_magic_number in
  let ic = open_in_bin fn in
  Misc.try_finally
    (fun () ->
       let buffer = really_input_string ic (String.length magic) in
       if buffer <> magic then
         raise (Error (Wrong_magic fn));
       input_value ic)
    (fun () -> close_in ic)

(* Error printing *)

let report_error ppf = function
  | Wrong_magic fn ->
      Format.fprintf ppf "File is not a valid binary parsetree@.\
                   Path: %s@." fn

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
