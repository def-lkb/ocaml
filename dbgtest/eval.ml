type runtime_state = {
  fp : Dbgprim.frame_pointer;
  ev : Instruct.debug_event;
  env : Env.t;
}

let symbols =
  match Symbol_loader.read_symbols Sys.executable_name with
  | Error msg ->
      prerr_endline msg;
      exit 1
  | Ok symbols -> symbols

let events_index = Hashtbl.create 7919

let runtime_state = ref None

let find_id id {Instruct. ce_stack; ce_heap; ce_rec} ss =
  match Ident.find_same id ce_stack with
  | pos -> `Stack (ss - pos)
  | exception Not_found ->
      match Ident.find_same id ce_heap with
      | pos -> `Heap pos
      | exception Not_found ->
          match Ident.find_same id ce_rec with
          | pos -> `Rec pos
          | exception Not_found -> `Unbound

let toplevel_get =
  let counter = ref 0 in
  let uniq () = incr counter; "_uniq_" ^ string_of_int !counter in
  fun id ->
    match !runtime_state with
    | None -> assert false
    | Some st ->
        match
          match find_id id st.ev.Instruct.ev_compenv st.ev.Instruct.ev_stacksize with
          | `Unbound -> None
          | `Stack pos -> Dbgprim.peek_stack st.fp pos
          | `Heap pos -> Dbgprim.peek_heap st.fp pos
          | `Rec pos -> Dbgprim.peek_rec st.fp pos
        with
        | None -> Translmod.call_toploop_getvalue (Translmod.toplevel_name id)
        | Some obj ->
            let name = uniq () in
            Toploop.toplevel_value_bindings :=
              Toploop.StringMap.add name obj !Toploop.toplevel_value_bindings;
            Translmod.call_toploop_getvalue name

let () =
  Config.load_path := !Config.load_path @ symbols.Symbol_loader.dirs

let () =
  List.iter (fun (_, events) ->
      List.iter (fun (event : Instruct.debug_event) ->
          Hashtbl.add events_index event.ev_pos event)
        events) symbols.Symbol_loader.events

let eval source =
  Dbgprim.with_stack begin function
  | None -> assert false
  | Some fp ->
      (*match Dbgprim.next fp with
      | None -> assert false
      | Some fp ->*)
          let ev =
            try Hashtbl.find events_index (Dbgprim.location fp)
            with Not_found -> assert false
          in
          begin try
            Envaux.reset_cache ();
            Ctype.reset_global_level ();
            let env =
              Envaux.env_from_summary
                ev.Instruct.ev_typenv
                ev.Instruct.ev_typsubst
            in
            Ctype.rigid_range_before :=
              Btype.generic_level + ev.Instruct.ev_typtime - 1;
            Ctype.rigid_range_after  :=
              Btype.generic_level + ev.Instruct.ev_typtime + 1000;
            let impl =
              Parser.implementation Lexer.token (Lexing.from_string source)
            in
            Ctype.set_levels { Ctype.
                               current_level = 10000;
                               nongen_level = 10000;
                               global_level = 10001;
                               saved_level = [] };
            let str, _sg, _env = Typemod.type_toplevel_phrase env impl in
            runtime_state := Some { fp; ev; env };
            Translmod.toploop_getvalue := toplevel_get;
            let lam = Translmod.transl_toplevel_definition str in
            let slam = Simplif.simplify_lambda "//eval//" lam in
            let init_code, fun_code = Bytegen.compile_phrase slam in
            let (code, code_size, reloc, events) =
              Emitcode.to_memory init_code fun_code
            in
            Meta.add_debug_info code code_size [| events |];
            Symtable.patch_object code reloc;
            Symtable.check_global_initialized reloc;
            Symtable.update_global_table ();
            let retval = (Meta.reify_bytecode code code_size) () in
            ignore retval
      with exn ->
        Location.report_exception Format.err_formatter exn
      end;
  end

let () =
  Toploop.initialize_toplevel_env ();
  Compmisc.init_path true

let evil str  =
  let r = ref 0 in
  eval str;
  !r

let () =
  print_int (evil "");
  print_newline ();
  print_int (evil "r := !r + 1");
  print_newline ()
