let next_env_summary : Env.summary -> Env.summary option = function
  | Env_empty -> None
  | Env_value (s, _, _) | Env_type (s, _, _) | Env_extension (s, _, _)
  | Env_module (s, _, _) | Env_modtype (s, _, _) | Env_class (s, _, _)
  | Env_cltype (s, _, _) | Env_open (s, _) | Env_functor_arg (s, _)
  | Env_constraints (s, _) | Env_copy_types (s, _) -> Some s

let rec unfold_summaries (x : Env.summary) : Env.summary list =
  match next_env_summary x with
  | None -> [x]
  | Some x' -> x :: unfold_summaries x'

let dump_env_summary ppf : Env.summary -> unit = function
  | Env_empty ->
    Format.fprintf ppf "Env_empty";
  | Env_value (_, id, vd) ->
    Format.fprintf ppf "Env_value (_, %a, %a)"
      Ident.print id Printer.placeholder vd
  | Env_type (_, id, td) ->
    Format.fprintf ppf "Env_type (_, %a, %a)"
      Ident.print id Printer.placeholder td
  | Env_extension (_, id, _) ->
    Format.fprintf ppf "Env_extension (_, %a, %a)"
      Ident.print id Printer.placeholder ()
  | Env_module (_, id, _) ->
    Format.fprintf ppf "Env_module (_, %a, %a)"
      Ident.print id Printer.placeholder ()
  | Env_modtype (_, id, _) ->
    Format.fprintf ppf "Env_modtype (_, %a, %a)"
      Ident.print id Printer.placeholder ()
  | Env_class (_, id, _) ->
    Format.fprintf ppf "Env_class (_, %a, %a)"
      Ident.print id Printer.placeholder ()
  | Env_cltype (_, id, _) ->
    Format.fprintf ppf "Env_cltype (_, %a, %a)"
      Ident.print id Printer.placeholder ()
  | Env_open (_, path) ->
    Format.fprintf ppf "Env_open (_, %a)"
      Printtyp.path path
  | Env_functor_arg (_, id) ->
    Format.fprintf ppf "Env_functor_arg (_, %a)"
      Ident.print id
  | Env_constraints (_, eqns) ->
    Format.fprintf ppf "Env_constraints (_, %a)"
      Printer.placeholder eqns
  | Env_copy_types (_, names) ->
    Format.fprintf ppf "Env_copy_types (_, %a)"
      (Printer.list (Printer.fmt "%S")) names

let dump_typed_env_summary ppf : Env.summary -> unit = function
  | Env_empty -> ()
  | Env_value (_, id, vd) ->
    Printtyp.value_description id ppf vd;
    Format.fprintf ppf "@ (* type = %a *)@ "
      Printtyp.raw_type_expr vd.val_type
  | Env_type (_, id, td) ->
    Printtyp.type_declaration id ppf td
  | Env_extension (_, id, ec) ->
    Printtyp.extension_constructor id ppf ec
  | Env_module (_, id, md) ->
    Format.fprintf ppf "module %s : %a"
      (Ident.name id) Printtyp.modtype md.md_type
  | Env_modtype (_, id, mtd) ->
    Printtyp.modtype_declaration id ppf mtd
  | Env_class (_, id, cd) ->
    Printtyp.class_declaration id ppf cd
  | Env_cltype (_, id, ctd) ->
    Printtyp.cltype_declaration id ppf ctd
  | Env_open (_, path) ->
    Format.fprintf ppf "open %a" Printtyp.path path
  | Env_functor_arg (_, id) ->
    Format.fprintf ppf "(* inside functor (%s : _) -> ... *)"
      (Ident.name id)
  | Env_constraints (_, eqns) ->
    Format.fprintf ppf "@[<v>(* equations :@ ";
    Env.PathMap.iter (fun path td ->
        Format.fprintf ppf "   In %a: %a@ "
          Printtyp.path path
          (Printtyp.type_declaration (Path.head path)) td
      ) eqns;
    Format.fprintf ppf "*)@]";
  | Env_copy_types (_, names) ->
    Format.fprintf ppf "(* copy types %a *)"
      (Printer.list (Printer.fmt "%S")) names

let dump_env_summaries ppf env =
  Format.fprintf ppf "Env_summary %a"
    (Printer.list dump_env_summary) (unfold_summaries env)

let todo x = x

let () =
  let myself = Sys.executable_name in
  match Symbol_loader.read_symbols myself with
  | Error msg ->
      prerr_endline msg;
      exit 1
  | Ok symbols ->
    Config.load_path := !Config.load_path @ symbols.dirs;
    let events_index = Hashtbl.create 7919 in
    List.iter (fun (_, events) ->
        List.iter (fun (event : Instruct.debug_event) ->
            Hashtbl.add events_index event.ev_pos event)
          events) symbols.events;
    (*Format.printf "%a\n%!" Symbol_loader.dump symbols;*)
    match (fun x -> x) with
    | _todo ->
        let k f y =
          (fun x ->
             Dbgprim.with_stack begin function
             | None -> prerr_endline "No debug information for stack frames"
             | fp ->
                 let rec print_fp = function
                   | Some fp ->
                       Format.printf "- FP = %d\n%!" (Dbgprim.location fp);
                       begin match Hashtbl.find events_index (Dbgprim.location fp) with
                       | exception Not_found ->
                           Format.eprintf "  Internal error: no debug information"
                       | ev ->
                           Format.printf "  Summary: @[%a@]\n%!"
                             (Printer.list dump_typed_env_summary)
                             (unfold_summaries ev.Instruct.ev_typenv);
                           begin try
                             Envaux.reset_cache ();
                             Ctype.reset_global_level ();
                             let env =
                               Envaux.env_from_summary
                                 ev.Instruct.ev_typenv
                                 ev.Instruct.ev_typsubst
                             in
                             let expr =
                               Parser.parse_expression Lexer.token
                                 (Lexing.from_string "y := !y + 1")
                             in
                             Ctype.set_levels { Ctype.
                                                current_level = 10000;
                                                nongen_level = 10000;
                                                global_level = 10001;
                                                saved_level = [] };
                             let _expr = Typecore.type_expression env expr in
                             ()
                           with exn ->
                             Location.report_exception Format.err_formatter exn
                           end;
                           (*let env =
                             Envaux.env_from_summary
                               ev.Instruct.ev_typenv
                               ev.Instruct.ev_typsubst
                             in
                             let summaries = unfold_summaries (Env.summary env) in
                             Format.printf "%a\n%!"
                             (Printer.list dump_typed_env_summary) summaries*)
                       end;
                       print_fp (Dbgprim.next fp)
                   | None -> ()
                 in
                 print_fp fp
             end;
             ignore x)
            ();
          f y
        in
        k print_int 42
