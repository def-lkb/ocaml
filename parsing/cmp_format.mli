type cmp_content =
  | Cmp_interface of Parsetree.signature
  | Cmp_implementation of Parsetree.structure

type cmp_infos = {
  cmp_content : cmp_content;
  cmp_output_prefix : string;
  cmp_source_file : string;
  cmp_module_name : string;
  cmp_input_name : string;
  cmp_comments : (string * Location.t) list;
}

val write_cmp_infos: cmp_infos -> string -> unit
val read_cmp_infos: string -> cmp_infos
