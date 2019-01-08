type t = {
  symtable: Symtable.global_map;
  events: (string * Instruct.debug_event list) list;
  dirs: string list;
}

let inspect_symtable : Symtable.global_map -> Ident.t Cmo_format.numtable =
  Obj.magic

let relocate_event orig (ev : Instruct.debug_event) =
  ev.ev_pos <- orig + ev.ev_pos;
  match ev.ev_repr with
  | Event_parent repr -> repr := ev.ev_pos
  | _ -> ()

let partition_modules evl =
  let table = Hashtbl.create 7 in
  List.iter (fun ev -> Hashtbl.add table ev.Instruct.ev_module ev) evl;
  Hashtbl.fold (fun ev_module ev -> function
      | (ev_module', evl) :: rest when ev_module' = ev_module ->
          (ev_module', (ev :: evl)) :: rest
      | acc -> (ev_module, [ev]) :: acc
    ) table []

let read_symbols_exn bytecode_file =
  let ic = open_in_bin bytecode_file in
  let symtable = match
      Bytesections.read_toc ic;
      ignore (Bytesections.seek_section ic "SYMB");
      input_value ic
    with
    | symtable -> symtable
    | exception (Bytesections.Bad_magic_number | Not_found) ->
        failwith (bytecode_file ^ " is not a bytecode file.")
  in
  let num_eventlists = match
      ignore (Bytesections.seek_section ic "DBUG");
      input_binary_int ic
    with
    | num_eventlists -> num_eventlists
    | exception Not_found ->
        failwith (bytecode_file ^ " has no debugging info.")
  in
  let eventlists = ref [] in
  let dirs = Hashtbl.create 7 in
  for _i = 1 to num_eventlists do
    let orig = input_binary_int ic in
    let evl = (input_value ic : Instruct.debug_event list) in
    (* Relocate events in event list *)
    List.iter (relocate_event orig) evl;
    eventlists := partition_modules evl @ !eventlists;
    List.iter (fun dir -> Hashtbl.replace dirs dir ()) (input_value ic);
  done;
  close_in_noerr ic;
  let dirs = Hashtbl.fold (fun x () xs -> x :: xs) dirs [] in
  { symtable; events = !eventlists; dirs }

let read_symbols bytecode_file =
  match read_symbols_exn bytecode_file with
  | symbol -> Ok symbol
  | exception Failure msg -> Error msg

let dump_symtable ppf symtable =
  let table = inspect_symtable symtable in
  let dump_tbl ppf tbl =
    Format.fprintf ppf "Tbl.[@[";
    Tbl.iter (fun k v -> Format.fprintf ppf "%a -> %d;@ " Ident.print k v) tbl;
    Format.fprintf ppf "@]]"
  in
  Format.fprintf ppf "{ @[num_cnt = %d;@ num_tbl = %a@] }"
    table.num_cnt dump_tbl table.num_tbl

let dump_events ppf =
  Printer.list (fun ppf (ev_module, evs) ->
      Format.fprintf ppf "(%S, %a)" ev_module
        (Printer.list Printer.debug_event) evs
    ) ppf

let dump ppf (t : t) =
  Format.fprintf ppf "@[{ @[symtable = %a;@ events = %a;@ dirs = %a@] }@]"
    dump_symtable t.symtable
    dump_events t.events
    (Printer.list (Printer.fmt "%S")) t.dirs
