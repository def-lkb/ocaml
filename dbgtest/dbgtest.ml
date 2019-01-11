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

type runtime_state = {
  fp : Dbgprim.frame_pointer;
  ev : Instruct.debug_event;
  env : Env.t;
}

let print_env = ref None

module Evp = struct
  type valu = Obj.t

  exception Error

  let rec eval_path' st = function
    | Path.Pident id ->
        begin match Ident.find_same id
                      st.ev.Instruct.ev_compenv.Instruct.ce_stack
        with
        | pos ->
            begin match Dbgprim.peek st.fp (2 + st.ev.Instruct.ev_stacksize - pos) with
            | None -> failwith "eval_path (Pident _) = invalid stack access"
            | Some obj -> obj
            end
        | exception Not_found ->
            Ident.iter (fun ident pos ->
                Format.eprintf "stack: %a -> %d\n%!" Printtyp.ident ident pos
              ) st.ev.Instruct.ev_compenv.Instruct.ce_stack;
            Ident.iter (fun ident pos ->
                Format.eprintf "heap: %a -> %d\n%!" Printtyp.ident ident pos
              ) st.ev.Instruct.ev_compenv.Instruct.ce_heap;
            Ident.iter (fun ident pos ->
                Format.eprintf "rec: %a -> %d\n%!" Printtyp.ident ident pos;
                begin match Dbgprim.peek_rec st.fp 2 with
                | None -> assert false
                | Some obj ->
                    prerr_endline (Obj.obj obj ())
                end
              ) st.ev.Instruct.ev_compenv.Instruct.ce_rec;
            begin match Dbgprim.peek st.fp 0 with
            | None -> failwith "eval_path (Pident _) = missing env"
            | Some env ->
                match Ident.find_same id st.ev.Instruct.ev_compenv.Instruct.ce_heap with
                | exception Not_found ->
                    failwith "eval_path (Pident _) = invalid heap access"
                | pos ->
                    Obj.field env pos
            end
        end
    | Path.Pdot (p, _, pos) ->
        Obj.field (eval_path' st p) pos
    | Path.Papply _ ->
        failwith "eval_path (Papply _) = TODO"

  let eval_path env path =
    match !print_env with
    | Some st -> eval_path' st path
    | None -> failwith "eval_path _ = runtime state is missing"

  let same_value = (==)
end

module Printval = Genprintval.Make(Obj)(Evp)

let outval_of_value steps depth path typ =
  match !print_env with
  | None -> Outcometree.Oval_ellipsis
  | Some st ->
      begin match
        Printval.outval_of_value
          steps depth (fun _ _ _ -> None) st.env (Evp.eval_path st.env path) typ
      with
      | tree -> tree
      | exception exn ->
          let s = Printexc.to_string exn in
          Outcometree.Oval_string (s, String.length s, Outcometree.Ostr_string)
      end

let dump_typed_env_summary ppf : Env.summary -> unit = function
  | Env_empty -> ()
  | Env_value (_, id, vd) ->
    let tree = outval_of_value 10 10 (Path.Pident id) vd.val_type in
    Format.fprintf ppf "@[@[%a@] =@ @[%a@]@]\n%!"
      (Printtyp.value_description id) vd !Oprint.out_value tree;
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
    let _todo x = x in
    (*Ctype.rigid_range_before := 100_002_621;
      Ctype.rigid_range_after  := 100_002_624;*)
    let rec k f (y : int) =
      (fun x ->
         Dbgprim.with_stack begin function
         | None -> prerr_endline "No debug information for stack frames"
         | fp ->
             Dbgprim.log g;
             let rec print_fp = function
               | Some fp ->
                   Format.printf "- FP = %d\n%!" (Dbgprim.location fp);
                   begin match Hashtbl.find events_index (Dbgprim.location fp) with
                   | exception Not_found ->
                       Format.eprintf "  Internal error: no debug information"
                   | ev ->
                       Format.printf "  Type time: %d\n" ev.Instruct.ev_typtime;
                       (*Format.printf "  Summary: @[%a@]\n%!"
                         (Printer.list dump_typed_env_summary)
                         (unfold_summaries ev.Instruct.ev_typenv);*)
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
                         let expr =
                           Parser.parse_expression Lexer.token
                             (Lexing.from_string "_todo y" (*"y := !y + 1"*))
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
                       let env =
                         Envaux.env_from_summary
                           ev.Instruct.ev_typenv
                           ev.Instruct.ev_typsubst
                         in
                         let summaries = unfold_summaries (Env.summary env) in
                         print_env := Some { env; ev; fp };
                         Format.printf "%a\n%!"
                           (Printer.list dump_typed_env_summary) summaries;
                         print_env := None
                   end;
                   print_fp (Dbgprim.next fp)
               | None -> ()
             in
             print_fp fp
         end;
         ignore (g ());
         ignore x)
        ();
      f y
    and g () = "POWER"
    in
    k print_int 42

