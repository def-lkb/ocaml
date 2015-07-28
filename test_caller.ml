external get_caller : unit -> int = "%getcaller"

let here = get_caller

let print_retloc loc =
  Printf.sprintf "%08x" (loc * 2)

let test () =
  let caller = get_caller () in
  let myself = here () in
  (* Build with -g, match addresses against objdump --dwarf=decodedline ./a.out *)
  Printf.printf "caller: %s\n" (print_retloc caller);
  Printf.printf "myself: %s\n" (print_retloc myself)

let () =
  test ()
