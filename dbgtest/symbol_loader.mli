type t = {
  symtable : Symtable.global_map;
  events : (string * Instruct.debug_event list) list;
  dirs : string list;
}

val inspect_symtable : Symtable.global_map -> Ident.t Cmo_format.numtable

val read_symbols_exn : string -> t
val read_symbols : string -> (t, string) result

val dump : t Printer.t
