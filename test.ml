type r = { a : int ; b : float }

(*let r a b = Sys.opaque_identity { a; b }

let descriptors = Obj.Tag_descriptor.read_self_descriptors ()

let print_desc {Obj.Tag_descriptor. tag; size; constructor; fields } =
  Printf.printf "  { tag = %d; size = %d; constructor = %S; fields = [%s] }\n%!"
    tag size constructor
    (String.concat ";" (List.map (Printf.sprintf "%S") fields))
*)
type ex = Ex : 'a -> ex

let challenge x = (Ex (Some [1;2;x]))

let () =
  (*Printf.printf "%d descriptors:\n"
    (List.length descriptors);
  List.iter print_desc descriptors;*)
  let r = { a = 0; b = 0.1 } in
  Var_dump.var_dump r;
  prerr_endline
    ("r profinfo: " ^ string_of_int (Obj.get_profinfo (Obj.repr r)));
  let x = Ex (Some [1;2;3]) in
  Var_dump.var_dump x;
  prerr_endline
    ("x profinfo: " ^ string_of_int (Obj.get_profinfo (Obj.repr x)));
  let r' = Marshal.from_string (Marshal.to_string r []) 0 in
  Var_dump.var_dump r';
  prerr_endline
    ("r' profinfo: " ^ string_of_int (Obj.get_profinfo (Obj.repr r')));
